from lark import Lark, Transformer, v_args, Tree, Token
from collections import Counter

# ---------------------------------------
#   여기에는 네가 이미 쓰고 있는 GRAMMAR 문자열이 온다고 가정
#   GRAMMAR = r""" ... """
# ---------------------------------------

# ====== Expression / Condition 부분: inline ======

@v_args(inline=True)
class ExprMixin(Transformer):
    # 숫자
    def number(self, tok):
        s = str(tok)
        if any(c in s for c in ".Ee"):
            return ('NUM', float(s))
        else:
            return ('NUM', int(s))

    # 문자열
    def string(self, tok):
        # ESCAPED_STRING 이라 양끝에 " 가 있음
        s = str(tok)
        if len(s) >= 2 and s[0] == s[-1] == '"':
            s = s[1:-1]
        return ('STR', s)

    # 변수
    def var(self, tok):
        return ('VAR', str(tok))

    # CALL-like (표현식 위치)
    def call_like(self, name, args=None):
        if isinstance(name, Token):
            name = str(name)
        if args is None:
            return ('CALL', name, [])
        # args는 이미 expr 리스트로 변환된 상태
        return ('CALL', name, list(args))

    def args(self, *items):
        return list(items)

    # 산술 이항연산
    def add(self, a, b): return ('BIN', '+', a, b)
    def sub(self, a, b): return ('BIN', '-', a, b)
    def mul(self, a, b): return ('BIN', '*', a, b)
    def div(self, a, b): return ('BIN', '/', a, b)
    def pow(self, a, b): return ('POW', a, b)

    # 단항
    def neg(self, x): return ('UN', '-', x)
    def pos(self, x): return ('UN', '+', x)

    # 비교 연산자
    def compop(self, op_tok):
        return str(op_tok)

    def rel_expr(self, *items):
        # expr compop expr  |  expr
        if len(items) == 3:
            left, op, right = items
            return ('CMP', op, left, right)
        else:
            # 그냥 expr 하나
            return items[0]

    # 논리식 (AND/OR)
    def or_(self, a, b):   # grammar에서 -> or 로 alias 했다고 가정
        return ('OR', a, b)

    def and_(self, a, b):  # grammar에서 -> and 로 alias
        return ('AND', a, b)

    def bool_expr(self, x):
        # 단일 하위 노드인 경우 그대로
        return x

    def cond(self, x):
        # 최종 조건 표현식
        return x


# ====== 전체 Transformer ======

class BasicTransformer(ExprMixin):
    """
    Tree -> IR 로 바꿔주는 Transformer.
    최종 출력: [(lineno, label, stmt), ...]
    """

    # --- 최상위 / 라인 구조 ---

    def start(self, children):
        # children: [line1, line2, ...]
        # 각 line: (lineno, label, stmt)
        return children

    def line(self, children):
        # line: NUMBER statement NEWLINE? -> line
        # children[0]: Token(NUMBER), children[1]: transformed statement
        lineno_tok = children[0]
        stmt = children[1]
        lineno = int(str(lineno_tok))

        label = None
        real_stmt = stmt

        # labeled_stmt 가 ('LABELED', label, inner_stmt) 형태로 반환되도록 할 예정
        if isinstance(stmt, tuple) and stmt and stmt[0] == 'LABELED':
            label = stmt[1]
            real_stmt = stmt[2]

        return (lineno, label, real_stmt)

    # statement: labeled_stmt | plain_stmt
    def statement(self, children):
        # 하나만 내려온다고 가정
        return children[0]

    # labeled_stmt: VAR COLON plain_stmt
    def labeled_stmt(self, children):
        label_tok = children[0]
        stmt = children[2] if len(children) > 2 else None
        return ('LABELED', str(label_tok), stmt)

    # plain_stmt: ... | empty_stmt
    def plain_stmt(self, children):
        return children[0] if children else ('NOP',)

    # empty_stmt: -> empty
    def empty(self, *args):
        return ('NOP',)

    # ---- DEF FN ----
    # def_fn: DEF_KW VAR LPAR [def_args] RPAR
    def def_fn(self, children):
        # children 예시: [DEF_KW, VAR, "(", def_args?, ")"]
        if len(children) == 4:
            # DEF, name, "(", ")"
            _, name_tok, _lpar, _rpar = children
            params = []
        else:
            # DEF, name, "(", def_args, ")"
            _, name_tok, _lpar, def_args_ir, _rpar = children
            params = def_args_ir

        name = str(name_tok)
        return ('DEF_FN_HDR', name, params)

    def def_args(self, children):
        # def_args: def_item (COMMA def_item)*
        # def_item이 list 또는 ('PARAM',name,type) 형태로 들어올 수 있음
        out = []
        for ch in children:
            if isinstance(ch, list):
                out.extend(ch)
            elif isinstance(ch, tuple):
                out.append(ch)
        return out

    # def_item: VAR | typed_group
    def def_item(self, children):
        node = children[0]
        if isinstance(node, tuple) or isinstance(node, list):
            return node
        # VAR 토큰이라고 가정
        name = str(node)
        # 타입 없는 경우 기본 REAL로 둔다거나, None으로 둔다거나
        return [('PARAM', name, None)]

    # typed_group: type_kw VAR (COMMA VAR)*
    def typed_group(self, children):
        # children: [type_kw, VAR, VAR, ...]
        t = children[0]   # type_kw에서 이미 문자열로 변환되도록 하는 편이 좋음
        if isinstance(t, Token):
            tname = str(t)
        else:
            tname = t
        params = []
        for tok in children[1:]:
            name = str(tok)
            params.append(('PARAM', name, tname))
        return params

    # type_kw: INTEGER_KW | REAL_KW  (여기서 문자열로 변환)
    @v_args(inline=True)
    def type_kw(self, tok):
        return str(tok)

    # ---- COM ----
    # com_stmt: "COM" SLASHNAME com_decls (COMMA com_decls)*
    def com_stmt(self, children):
        # children: ["COM", SLASHNAME, com_decls, (COMMA, com_decls)...]
        block_tok = children[1]
        block_name = str(block_tok)
        # /Port/ -> Port 처럼 슬래시 제거
        if block_name.startswith('/') and block_name.endswith('/'):
            block_name = block_name[1:-1]

        decls = []
        for ch in children[2:]:
            if isinstance(ch, list):
                decls.extend(ch)
        return ('COM', block_name, decls)

    # com_decls: type_kw (VAR | array_any)
    def com_decls(self, children):
        tname = children[0]
        target = children[1]
        if isinstance(target, tuple):
            # array_any 같은 걸 ('ARRAY_ALL', name) 형식으로 만든다고 가정
            return [('COM_DECL', tname, target)]
        else:
            # VAR 토큰
            return [('COM_DECL', tname, ('VAR', str(target)))]

    # array_any: VAR LPAR MUL RPAR
    def array_any(self, children):
        name_tok = children[0]
        return ('ARRAY_ALL', str(name_tok))

    # ---- INTEGER 선언 ----
    # integer_decl: INTEGER_KW var_list
    def integer_decl(self, children):
        vars_ir = children[1] if len(children) > 1 else []
        return ('INTEGER_DECL', vars_ir)

    def var_list(self, children):
        # VAR (COMMA VAR)*
        return [ ('VAR', str(tok)) for tok in children ]

    # ---- ALLOCATE / DEALLOCATE ----
    # allocate_stmt: "ALLOCATE" type_kw alloc_list
    def allocate_stmt(self, children):
        tname = children[1]
        allocs = children[2] if len(children) > 2 else []
        return ('ALLOCATE', tname, allocs)

    def alloc_list(self, children):
        return children

    # alloc_item: VAR LPAR args RPAR
    def alloc_item(self, children):
        name_tok = children[0]
        args_ir = children[2] if len(children) > 2 else []
        return ('ALLOC_ITEM', str(name_tok), args_ir)

    # deallocate_stmt: "DEALLOCATE" dealloc_list
    def deallocate_stmt(self, children):
        lst = children[1] if len(children) > 1 else []
        return ('DEALLOCATE', lst)

    def dealloc_list(self, children):
        return children

    # dealloc_item: VAR LPAR MUL RPAR
    def dealloc_item(self, children):
        name_tok = children[0]
        return ('DEALLOC_ITEM', str(name_tok))

    # ---- FOR / NEXT ----
    # for_stmt: "FOR" VAR EQ expr "TO" expr ("STEP" expr)?
    def for_stmt(self, children):
        # children: ["FOR", VAR, "=", expr, "TO", expr, ("STEP",expr)?]
        var_tok = children[1]
        start = children[3]
        end = children[5]
        step = ('NUM', 1)  # default
        if len(children) > 6:
            step = children[7]
        return ('FOR', str(var_tok), start, end, step)

    # next_stmt: "NEXT" VAR
    def next_stmt(self, children):
        var_tok = children[1]
        return ('NEXT', str(var_tok))

    # ---- SELECT / CASE ----
    def select_stmt(self, children):
        expr = children[1]  # "SELECT", expr
        return ('SELECT', expr)

    def case_stmt(self, children):
        # CASE case_vals
        vals = children[1]
        if vals == ('CASE_ELSE',):
            return ('CASE_ELSE',)
        else:
            return ('CASE', vals)

    # case_vals: "ELSE" | case_list
    def case_vals(self, children):
        if len(children) == 1 and isinstance(children[0], Token) and str(children[0]) == 'ELSE':
            return ('CASE_ELSE',)
        else:
            return children[0]  # case_list

    # case_list: expr (COMMA expr)*
    def case_list(self, children):
        return list(children)

    def end_select_stmt(self, children):
        return ('END_SELECT',)

    # ---- IF ----
    # if_then_inline: "IF" cond "THEN" inline_tail
    def if_then_inline(self, children):
        # children: ["IF", cond, "THEN", inline_tail]
        cond_ir = children[1]
        tail = children[3]  # inline_tail (Tree or list)
        return ('IF_INLINE', cond_ir, tail)

    # if_block_hdr: "IF" cond "THEN"
    def if_block_hdr(self, children):
        cond_ir = children[1]
        return ('IF_HDR', cond_ir)

    def else_stmt(self, children):
        return ('ELSE',)

    def end_if_stmt(self, children):
        return ('END_IF',)

    # inline_tail: non_nl_token+
    def inline_tail(self, children):
        # 일단 토큰/노드를 그대로 보존 (2차 패스에서 재해석)
        return children

    # ---- ERROR ----
    def on_error_goto(self, children):
        # "ON" "ERROR" "GOTO" (LABEL | NUMBER)
        target = children[-1]
        if isinstance(target, Token):
            s = str(target)
            if s.isdigit():
                target = int(s)
            else:
                target = s
        return ('ON_ERROR_GOTO', target)

    def off_error(self, children):
        return ('OFF_ERROR',)

    # ---- MASS STORAGE ----
    def mass_storage(self, children):
        path_tok = children[-1]
        s = str(path_tok)
        if len(s) >= 2 and s[0] == s[-1] == '"':
            s = s[1:-1]
        return ('MASS_STORAGE', s)

    # ---- ASSIGN / OUTPUT / CREATE / PURGE ----
    def assign_device_stmt(self, children):
        # "ASSIGN" DEVICE "TO" assign_target (SEMI assign_opts)?
        device_tok = children[1]
        target = children[3]
        opts = None
        if len(children) > 4:
            # children[4]는 ';' 또는 assign_opts 트리 등 구조에 따라 조금 달라짐
            # 여기서는 대강 IR만 남겨두고, 나중에 세부 옵션 처리해도 됨
            for ch in children[4:]:
                if isinstance(ch, Tree) or isinstance(ch, tuple):
                    opts = ch
                    break
        return ('ASSIGN', str(device_tok), target, opts)

    def assign_target(self, children):
        node = children[0]
        if isinstance(node, Token):
            if str(node) == '*':
                return ('TARGET_STAR',)
            return ('TARGET_VAR', str(node))
        return node

    def output_stmt(self, children):
        # "OUTPUT" DEVICE SEMI output_tail
        device_tok = children[1]
        items = children[3] if len(children) > 3 else []
        return ('OUTPUT', str(device_tok), items)

    def output_tail(self, children):
        return list(children)

    def create_stmt(self, children):
        # "CREATE" VAR COMMA NUMBER
        name_tok = children[1]
        num_tok = children[3]
        n = int(str(num_tok))
        return ('CREATE', str(name_tok), ('NUM', n))

    def purge_stmt(self, children):
        name_tok = children[1]
        return ('PURGE', str(name_tok))

    # ---- RETURN / FNEND ----
    def return_stmt(self, children):
        expr = children[1] if len(children) > 1 else None
        return ('RETURN', expr)

    def fnend_stmt(self, children):
        return ('FNEND',)

    # ---- CALL / 할당 ----
    # call_stmt: "CALL" VAR (LPAR [args] RPAR)?
    def call_stmt(self, children):
        # "CALL", VAR, (LPAR, args?, RPAR)?
        name_tok = children[1]
        name = str(name_tok)
        args = []
        if len(children) > 2:
            # children[2]는 '(' 또는 args 트리일 수 있음 → grammar 구조에 따라 조정 필요
            for ch in children[2:]:
                if isinstance(ch, list):
                    args = ch
        return ('CALL_STMT', name, args)

    # call_like_stmt: call_like
    def call_like_stmt(self, children):
        node = children[0]
        # call_like에서 ('CALL', name, args) 형으로 변환된다고 가정
        if isinstance(node, tuple) and node[0] == 'CALL':
            name = node[1]
            args = node[2]
            return ('CALL_STMT', name, args)
        return node

    # bare_call_stmt: VAR -> bare_call
    def bare_call(self, tok):
        return ('BARE_CALL', str(tok))

    # assignment: VAR LPAR args RPAR EQ expr -> assign_idx
    def assign_idx(self, children):
        name_tok = children[0]
        args_ir = children[2]
        expr = children[4]
        return ('ASSIGN_IDX', str(name_tok), args_ir, expr)

    # assignment: VAR EQ expr -> assign_var
    def assign_var(self, children):
        name_tok = children[0]
        expr = children[2]
        return ('ASSIGN_VAR', str(name_tok), expr)


# ====== 테스트용: 파서 + Transformer 실행 예시 ======

def debug_parse_and_transform(basic_code, grammar):
    parser = Lark(grammar, parser="lalr", start="start", lexer="basic")
    tree = parser.parse(basic_code)

    print("=== Raw parse tree (top) ===")
    line_nodes = [ch for ch in tree.children if isinstance(ch, Tree) and ch.data == 'line']
    print(f"Total lines: {len(line_nodes)}")
    cnt = Counter(ch.children[1].data if isinstance(ch.children[1], Tree) else 'other'
                  for ch in line_nodes)
    print("Top-level stmt kinds:", cnt)

    ir = BasicTransformer().transform(tree)
    print("\n=== Transformed IR (first few lines) ===")
    for ln, label, stmt in ir[:10]:
        print(f"{ln:5d} label={label!r}  stmt={stmt}")

    return ir