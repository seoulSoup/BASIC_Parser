from lark import Lark, Transformer, Tree, Token
from lark.exceptions import UnexpectedInput
from collections import Counter

# --------------------------------------------------
#   BasicTransformer : parse tree -> IR
# --------------------------------------------------

class BasicTransformer(Transformer):
    """
    Lark parse tree를 해석하기 쉬운 튜플 기반 IR로 변환.
    최종 출력: [(lineno, label, stmt), ...]
      - lineno: int
      - label : "Sjy" 같은 라벨 이름 또는 None
      - stmt  : ('ASSIGN_VAR', ...), ('FOR', ...), ('IF_HDR', ...), ...
    """

    # ===== 공통 헬퍼 =====

    def _tok_str(self, tok):
        return str(tok)

    def _tok_int(self, tok):
        return int(str(tok))

    # ===== 최상위 / 라인 구조 =====

    def start(self, children):
        # children: [line1, line2, ...]
        return children

    def line(self, children):
        # line: NUMBER statement NEWLINE?  -> line
        lineno_tok = children[0]
        stmt = children[1]

        lineno = self._tok_int(lineno_tok)
        label = None
        real_stmt = stmt

        # labeled_stmt 가 ('LABELED', label, inner_stmt) 형태로 온다
        if isinstance(stmt, tuple) and stmt and stmt[0] == 'LABELED':
            label = stmt[1]
            real_stmt = stmt[2]

        return (lineno, label, real_stmt)

    def statement(self, children):
        # labeled_stmt | plain_stmt
        return children[0]

    def labeled_stmt(self, children):
        # labeled_stmt: VAR COLON plain_stmt
        label_tok = children[0]
        stmt = children[2] if len(children) > 2 else ('NOP',)
        return ('LABELED', self._tok_str(label_tok), stmt)

    def plain_stmt(self, children):
        # 여러 가지 stmt 중 하나. 이미 하위 메소드에서 튜플로 변환됨.
        return children[0] if children else ('NOP',)

    def empty(self, children):
        # empty_stmt: -> empty
        return ('NOP',)

    # ===== 숫자/문자/변수/기본 expr =====

    def number(self, children):
        tok = children[0]
        s = self._tok_str(tok)
        if any(c in s for c in '.Ee'):
            return ('NUM', float(s))
        else:
            return ('NUM', int(s))

    def string(self, children):
        tok = children[0]
        s = self._tok_str(tok)
        if len(s) >= 2 and s[0] == s[-1] == '"':
            s = s[1:-1]
        return ('STR', s)

    def var(self, children):
        tok = children[0]
        return ('VAR', self._tok_str(tok))

    # ----- expr 계층 -----

    def add(self, children):
        a, b = children
        return ('BIN', '+', a, b)

    def sub(self, children):
        a, b = children
        return ('BIN', '-', a, b)

    def mul(self, children):
        a, b = children
        return ('BIN', '*', a, b)

    def div(self, children):
        a, b = children
        return ('BIN', '/', a, b)

    def pow(self, children):
        a, b = children
        return ('POW', a, b)

    def neg(self, children):
        (x,) = children
        return ('UN', '-', x)

    def pos(self, children):
        (x,) = children
        return ('UN', '+', x)

    def expr(self, children):
        # ?expr: ... | term
        # term만 있는 경우 그대로 전달
        return children[0]

    def term(self, children):
        return children[0]

    def unary(self, children):
        return children[0]

    def primary(self, children):
        # "( expr )" 인 경우만 여기로 온다. number/string/call_like/var 는 별도 label 이미 처리됨.
        if len(children) == 3:
            # LPAR expr RPAR
            return children[1]
        else:
            return children[0]

    # ===== 비교 / 논리식 =====

    def compop(self, children):
        tok = children[0]
        return self._tok_str(tok)

    def rel_expr(self, children):
        # rel_expr: expr compop expr | expr
        if len(children) == 3:
            left, op, right = children
            return ('CMP', op, left, right)
        else:
            return children[0]

    def bool_expr(self, children):
        # OR/AND 없는 경우만 여기서 처리된다 (있으면 'or','and' alias로 감)
        return children[0]

    def cond(self, children):
        # cond: bool_expr
        return children[0]

    # Lark의 alias '-> or', '-> and' 를 처리 (메소드 이름을 쓸 수 없으므로 __default__에서 후킹)
    def __default__(self, data, children, meta):
        if data == 'or':
            left, right = children
            return ('OR', left, right)
        if data == 'and':
            left, right = children
            return ('AND', left, right)
        # 나머지는 기본 동작 (Tree 유지)
        return Transformer.__default__(self, data, children, meta)

    # ===== DEF FN 관련 =====

    def def_fn(self, children):
        # def_fn: DEF_KW VAR LPAR [def_args] RPAR
        # children 예시: [DEF_KW, VAR, "(", def_args?, ")"]
        def_kw = children[0]   # 안 씀
        name_tok = children[1]
        name = self._tok_str(name_tok)

        # def_args 유무
        params = []
        for ch in children[2:]:
            if isinstance(ch, list):
                params = ch

        return ('DEF_FN_HDR', name, params)

    def def_args(self, children):
        # def_args: def_item (COMMA def_item)*
        out = []
        for ch in children:
            if isinstance(ch, list):
                out.extend(ch)
            else:
                out.append(ch)
        return out

    def def_item(self, children):
        # def_item: typed_group | VAR
        node = children[0]
        if isinstance(node, list):
            return node   # typed_group 결과
        if isinstance(node, Token):
            # 타입 없는 파라미터
            return [('PARAM', self._tok_str(node), None)]
        return [node]

    def typed_group(self, children):
        # typed_group: type_kw (VAR | array_any)
        t = children[0]
        if isinstance(t, Token):
            tname = self._tok_str(t)
        else:
            tname = t

        params = []
        for target in children[1:]:
            if isinstance(target, Token):   # VAR
                params.append(('PARAM', self._tok_str(target), tname))
            elif isinstance(target, tuple) and target[0] == 'ARRAY_ALL':
                params.append(('PARAM_ARRAY', target[1], tname))
        return params

    def type_kw(self, children):
        tok = children[0]
        return self._tok_str(tok)

    # ===== COM / COMMON =====

    def com_stmt(self, children):
        # com_stmt: "COM" SLASHNAME com_decls (COMMA com_decls)*
        slash_name = self._tok_str(children[1])
        block_name = slash_name
        if block_name.startswith('/') and block_name.endswith('/'):
            block_name = block_name[1:-1]

        decls = []
        for ch in children[2:]:
            if isinstance(ch, list):
                decls.extend(ch)
            else:
                decls.append(ch)

        # decls 원소 예: ('COM_DECL', type, ('VAR',name)) 등으로 가공
        fixed = []
        for d in decls:
            if isinstance(d, tuple) and d[0] == 'COM_DECL':
                fixed.append(d)
            elif isinstance(d, list):
                fixed.extend(d)
            else:
                fixed.append(d)
        return ('COM', block_name, fixed)

    def com_decls(self, children):
        # com_decls: typed_group | array_any | VAR
        node = children[0]
        if isinstance(node, list):
            # typed_group 결과: [('PARAM', name, tname), ...]
            out = []
            for kind, name, tname in node:
                if kind == 'PARAM':
                    out.append(('COM_DECL', tname, ('VAR', name)))
                elif kind == 'PARAM_ARRAY':
                    out.append(('COM_DECL_ARRAY', tname, ('ARRAY_ALL', name)))
            return out
        if isinstance(node, tuple) and node[0] == 'ARRAY_ALL':
            # 타입 정보 없는 배열 (COM /Stray_cap/ ... REAL Cap_value(*), ...) 같은 건
            return [('COM_DECL_ARRAY', None, node)]
        if isinstance(node, Token):
            # 타입 없는 단일 변수
            return [('COM_DECL', None, ('VAR', self._tok_str(node)))]
        return [node]

    def array_any(self, children):
        # VAR LPAR MUL RPAR
        name_tok = children[0]
        return ('ARRAY_ALL', self._tok_str(name_tok))

    # ===== INTEGER 선언 =====

    def integer_decl(self, children):
        # INTEGER_KW var_list
        vars_ir = children[1] if len(children) > 1 else []
        return ('INTEGER_DECL', vars_ir)

    def var_list(self, children):
        return [('VAR', self._tok_str(tok)) for tok in children]

    # ===== ALLOC / DEALLOC =====

    def allocate_stmt(self, children):
        # "ALLOCATE" type_kw alloc_list
        tname = children[1]
        allocs = children[2] if len(children) > 2 else []
        return ('ALLOCATE', tname, allocs)

    def alloc_list(self, children):
        return children

    def alloc_item(self, children):
        # VAR LPAR args RPAR
        name_tok = children[0]
        args = []
        for ch in children[1:]:
            if isinstance(ch, list):
                args = ch
        return ('ALLOC_ITEM', self._tok_str(name_tok), args)

    def deallocate_stmt(self, children):
        # "DEALLOCATE" dealloc_list
        lst = children[1] if len(children) > 1 else []
        return ('DEALLOCATE', lst)

    def dealloc_list(self, children):
        return children

    def dealloc_item(self, children):
        name_tok = children[0]
        return ('DEALLOC_ITEM', self._tok_str(name_tok))

    # ===== FOR / NEXT =====

    def for_stmt(self, children):
        # "FOR" VAR EQ expr "TO" expr ("STEP" expr)?
        var_tok = children[1]
        start = children[3]
        end = children[5]
        step = ('NUM', 1)
        if len(children) > 6:
            step = children[7]
        return ('FOR', self._tok_str(var_tok), start, end, step)

    def next_stmt(self, children):
        var_tok = children[1]
        return ('NEXT', self._tok_str(var_tok))

    # ===== SELECT / CASE =====

    def select_stmt(self, children):
        expr = children[1]
        return ('SELECT', expr)

    def case_stmt(self, children):
        vals = children[1]
        if vals == ('CASE_ELSE',):
            return ('CASE_ELSE',)
        else:
            return ('CASE', vals)

    def case_vals(self, children):
        # "ELSE" | case_list
        if len(children) == 1 and isinstance(children[0], Token) and self._tok_str(children[0]) == 'ELSE':
            return ('CASE_ELSE',)
        else:
            return children[0]

    def case_list(self, children):
        return children

    def end_select_stmt(self, children):
        return ('END_SELECT',)

    # ===== IF =====

    def if_then_inline(self, children):
        # "IF" cond "THEN" inline_tail
        cond_ir = children[1]
        tail = children[3]
        return ('IF_INLINE', cond_ir, tail)

    def inline_tail(self, children):
        # non_nl_token+ => 토큰/노드 그대로 보존
        return children

    def if_block_hdr(self, children):
        # "IF" cond "THEN"
        cond_ir = children[1]
        return ('IF_HDR', cond_ir)

    def else_stmt(self, children):
        return ('ELSE',)

    def end_if_stmt(self, children):
        return ('END_IF',)

    # ===== ERROR HANDLING =====

    def on_error_goto(self, children):
        # "ON" "ERROR" "GOTO" (VAR | NUMBER)
        target_tok = children[3]
        s = self._tok_str(target_tok)
        if s.isdigit():
            target = int(s)
        else:
            target = s
        return ('ON_ERROR_GOTO', target)

    def off_error(self, children):
        return ('OFF_ERROR',)

    # ===== MASS STORAGE =====

    def mass_storage(self, children):
        # "MASS" "STORAGE" "IS" STRING
        str_tok = children[3]
        s = self._tok_str(str_tok)
        if len(s) >= 2 and s[0] == s[-1] == '"':
            s = s[1:-1]
        return ('MASS_STORAGE', s)

    # ===== ASSIGN / OUTPUT / CREATE / PURGE =====

    def assign_device_stmt(self, children):
        # "ASSIGN" DEVICE "TO" assign_target (SEMI assign_opts)?
        device_tok = children[1]
        device = self._tok_str(device_tok)
        target = None
        opts = None
        for ch in children[2:]:
            if isinstance(ch, tuple) and ch[0] in ('TARGET_VAR', 'TARGET_STAR'):
                target = ch
            elif isinstance(ch, tuple) and ch[0] == 'ASSIGN_OPTS':
                opts = ch
        return ('ASSIGN', device, target, opts)

    def assign_target(self, children):
        tok = children[0]
        if isinstance(tok, Token):
            if tok.type == 'MUL':
                return ('TARGET_STAR',)
            return ('TARGET_VAR', self._tok_str(tok))
        return tok

    def assign_opts(self, children):
        # FORMAT ON, EOL call_like, APPEND 등은 일단 토큰 그대로 보관
        return ('ASSIGN_OPTS', children)

    def output_stmt(self, children):
        # "OUTPUT" DEVICE SEMI output_tail
        device_tok = children[1]
        device = self._tok_str(device_tok)
        tail = children[3] if len(children) > 3 else []
        return ('OUTPUT', device, tail)

    def output_tail(self, children):
        # expr ((COMMA | SEMI) expr)*  => expr만 추려서 보냄
        out = []
        for ch in children:
            if isinstance(ch, tuple):
                out.append(ch)
        return out

    def create_stmt(self, children):
        # "CREATE" VAR COMMA NUMBER
        name_tok = children[1]
        num_tok = children[3]
        n = self._tok_int(num_tok)
        return ('CREATE', self._tok_str(name_tok), ('NUM', n))

    def purge_stmt(self, children):
        name_tok = children[1]
        return ('PURGE', self._tok_str(name_tok))

    # ===== RETURN / FNEND =====

    def return_stmt(self, children):
        expr = children[1] if len(children) > 1 else None
        return ('RETURN', expr)

    def fnend_stmt(self, children):
        return ('FNEND',)

    # ===== CALL / 호출문 / ASSIGNMENT =====

    def args(self, children):
        return children

    def call_like(self, children):
        # VAR LPAR [args] RPAR
        name_tok = children[0]
        name = self._tok_str(name_tok)
        args = []
        for ch in children[1:]:
            if isinstance(ch, list):
                args = ch
        return ('CALL', name, args)

    def call_like_stmt(self, children):
        # call_like
        node = children[0]
        if isinstance(node, tuple) and node[0] == 'CALL':
            name, args = node[1], node[2]
            return ('CALL_STMT', name, args)
        return node

    def call_stmt(self, children):
        # "CALL" VAR (LPAR [args] RPAR)?
        name = None
        args = []
        for ch in children:
            if isinstance(ch, Token) and ch.type == 'VAR':
                name = self._tok_str(ch)
            elif isinstance(ch, list):
                args = ch
        return ('CALL_STMT', name, args)

    def bare_call_stmt(self, children):
        # VAR
        tok = children[0]
        return ('BARE_CALL', self._tok_str(tok))

    def assign_idx(self, children):
        # VAR LPAR args RPAR EQ expr -> assign_idx
        name_tok = children[0]
        args = children[2]
        expr = children[4]
        return ('ASSIGN_IDX', self._tok_str(name_tok), args, expr)

    def assign_var(self, children):
        # VAR EQ expr -> assign_var
        name_tok = children[0]
        expr = children[2]
        return ('ASSIGN_VAR', self._tok_str(name_tok), expr)


# --------------------------------------------------
# parser + transformer 테스트용 함수 예시
# --------------------------------------------------

def parse_and_transform(code):
    parser = Lark(GRAMMAR, parser="lalr", start="start", lexer="basic")
    tree = parser.parse(code)
    print("✅ PARSE OK")

    # 간단 통계
    line_nodes = [n for n in tree.children if isinstance(n, Tree) and n.data == "line"]
    print("Lines parsed:", len(line_nodes))

    ir = BasicTransformer().transform(tree)
    print("=== IR first 10 lines ===")
    for ln, label, stmt in ir[:10]:
        print(f"{ln:5d}  label={label!r}  stmt={stmt}")
    return ir