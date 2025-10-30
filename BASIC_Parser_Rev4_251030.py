# -*- coding: utf-8 -*-
"""
HP BASIC(개조 문법 일부) 파서 + Transformer + 블록결합(2nd pass) + 요약
필요 패키지: pip install lark
실행: python script.py
"""

from typing import List, Tuple, Any
from collections import Counter
from lark import Lark, Transformer, v_args, Tree, Token

# -----------------------------
# 1) 예시 BASIC 코드 (내장)
# -----------------------------
BASIC_CODE = r"""62568 DEF FNCap_hpl4_nf2(Type$,INTEGER High_pad,Lo,Lo2,Lo3,Lo4,REAL Vstart,Vstop,Vstep,Vd1,Cap_frequency,Osclevel,D_time,REAL Out_var2,Out_var)
62569   !-,1~+44,-1~+44,-1~+44,-1~+44,-1~+44,-100~+100,-100~+100,-100~+100,-100~+100,+20~+1000000,+,.001~300,-,-
62570   !-------------------------------------------------
62571   !- Except Stray Cap. at meas. cap
62572   !-------------------------------------------------
62573   COM /Port/ INTEGER Port(*)
62574   COM /Pin_table/ INTEGER P(*)
62575   COM /Cap/ INTEGER Capflag,REAL Cap,S_para
62576   COM /Tag/ INTEGER Tag,Tag1
62577   COM /Points/ INTEGER Wa,Po,No
62578   COM /Info/ Devicename$,T_sys$,Run_no$,Wf$,Employee$,Listfile$,Wafer_no$(*),Rwf$,Sheet_no$,Cmts$,Pgm$,Sdate$
62579   COM /Stray_cap/ INTEGER Cap_count,Switch_box,Lock,Integer_a,REAL Cap_value(*),Item_value(*),Dummy_value(*),Real_a
62580   !
62581   INTEGER N,Icount
62582   Icount=ABS(INT((Vstop-Vstart)/Vstep))+1
62583   !
62584   ALLOCATE REAL Cv(Icount,2),Te_cv(Icount,3),Volts(Icount),Disp_f(Icount),G_value(Icount)
62585   !
62586   !-------------------------------------------------
62587   !- DUT Connection
62588   !-------------------------------------------------
62589   !
62590   FOR I=1 TO 44
62591     CALL Connect(FNPort(0,3),P(I))
62592   NEXT I
62593   !
62594   !
62595   SELECT High_pad
62596   CASE 1
62597     FOR I=1 TO 44
62598       SELECT I
62599       CASE 20,21,22,23,24,25,26
62600         Connect(FNPort(1,7),P(I))
62601       END SELECT
62602     NEXT I
62603   !
62604   CASE 2
62605     FOR I=1 TO 44
62606       SELECT I
62607       CASE 20,21,22,23,24,25
62608         Connect(FNPort(1,7),P(I))
62609       END SELECT
62610     NEXT I
62611   CASE 3
62612     FOR I=1 TO 44
62613       SELECT I
62614       CASE 18,19,20,21,22,23,29
62615         Connect(FNPort(1,7),P(I))
62616       END SELECT
62617     NEXT I
62618   CASE 4
62619     FOR I=1 TO 44
62620       SELECT I
62621       CASE 18,19,20,21,22,23
62622         Connect(FNPort(1,7),P(I))
62623       END SELECT
62624     NEXT I
62625   !
62626   !
62627   END SELECT
62628   !
62629   Connect(FNPort(1,8),Lo)
62630   IF Lo2>0 THEN CALL Connect(FNPort(1,8),Lo2)
62631   IF Lo3>0 THEN CALL Connect(FNPort(1,8),Lo3)
62632   IF Lo4>0 THEN CALL Connect(FNPort(1,8),Lo4)
62633   !
62634   Set_cmu(2,0,Osclevel)
62635   Frequency(Cap_frequency)
62636   IF Lock=0 THEN 
62637     CALL Offsetcap
62638   ELSE
62639     Cap_count=Cap_count+1
62640     S_para=Cap_value(Cap_count)
62641   END IF
62642   !
62643   !============================================================
62644   MASS STORAGE IS "/users/epm/data/r3eng/rawdata_r3epm"
62645   ON ERROR GOTO Sjy
62646 Sjy:IF FNFile$=FNFile$ AND Wa=1 AND Po=1 AND No=1 THEN 
62647     ON ERROR GOTO Sjy1
62648     PURGE FNFile$
62649     OFF ERROR 
62650 Sjy1:OFF ERROR 
62651   END IF
62652   ON ERROR GOTO 62657
62653   CREATE FNFile$,1
62654   ASSIGN @Path TO FNFile$;FORMAT ON,EOL CHR$(10),APPEND
62655   OUTPUT @Path;FNFile$
62656   ASSIGN @Path TO *
62657   OFF ERROR 
62658   ASSIGN @Path TO FNFile$;FORMAT ON,EOL CHR$(10),APPEND
62659   OUTPUT @Path;"++++++++++ Point";Po;"++++++++++++"
62660   ! OUTPUT @Path;"++++++++++ Wn_no";Wa;"++++++++++++"
62661   OUTPUT @Path;"++++++++++";Type$;"++++++++++++"
62662   OUTPUT @Path;"Gate_v, Cap, D_factor, Conductance"
62663   Force_v(FNPort(0,3),Vd1,Vd1,1.E-2)
62664   !
62665   I=0
62666   FOR Vtemp=Vstart TO Vstop+(Vstep/10.) STEP Vstep
62667     I=I+1
62668     Force_v(FNPort(1,7),Vtemp)
62669     Wait_th(D_time)
62670     Measure_cpg(Te_cv(I,1),G_value(I))
62671     Volts(I)=Vtemp
62672   NEXT Vtemp
62673   !
62674   FOR I=1 TO Icount
62675     Cv(I,1)=Te_cv(I,1)-S_para
62676   !
62677     ON ERROR GOTO Exit_disp
62678     Disp_f(I)=G_value(I)/(2*PI*Cap_frequency*Cv(I,1))
62679 Exit_disp:OFF ERROR 
62680   !
62681     OUTPUT @Path;Volts(I),Cv(I,1),Disp_f(I),G_value(I)
62682   NEXT I
62683   ASSIGN @Path TO *
62684   Tag=1
62685   !
62686   Disable_port
62687   Connect
62688   !
62689   Out_var=Cv(1,1)
62690   Out_var2=Cv(Icount,1)
62691   RETURN Cv(1,1)
62692   DEALLOCATE Cv(*),Te_cv(*),Volts(*),Disp_f(*),G_value(*)
62693 FNEND
"""

# -----------------------------
# 2) Lark 문법 (lexer="basic")
# -----------------------------
GRAMMAR = r"""
start: line+

line: NUMBER statement NEWLINE?      -> line

statement: labeled_stmt
         | plain_stmt

labeled_stmt: LABEL ":" plain_stmt
LABEL: /[A-Za-z_][A-Za-z0-9_]*/

plain_stmt: def_fn
          | com_stmt
          | integer_decl
          | allocate_stmt
          | deallocate_stmt
          | for_stmt
          | next_stmt
          | select_stmt
          | case_stmt
          | end_select_stmt
          | if_then_inline
          | if_block_hdr
          | else_stmt
          | end_if_stmt
          | on_error_goto
          | off_error
          | mass_storage
          | assign_device_stmt
          | output_stmt
          | create_stmt
          | purge_stmt
          | call_stmt
          | assignment
          | comment_stmt
          | empty_stmt

// ---- DEF FN ----
def_fn: DEF_KW VAR LPAR [def_args] RPAR
DEF_KW: "DEF"
LPAR: "("
RPAR: ")"

def_args: def_item (COMMA def_item)*
def_item: VAR | typed_group
typed_group: TYPE VAR (COMMA VAR)*

TYPE: INTEGER_KW | REAL_KW
INTEGER_KW: "INTEGER"
REAL_KW: "REAL"
COMMA: ","

// ---- COM ----
com_stmt: "COM" "/" NAME "/" com_decls (COMMA com_decls)*
com_decls: TYPE (VAR | array_any)
array_any: VAR LPAR STAR RPAR
STAR: "*"

// ---- DECL ----
integer_decl: INTEGER_KW var_list
var_list: VAR (COMMA VAR)*

// ---- ALLOC / DEALLOC ----
allocate_stmt: "ALLOCATE" TYPE alloc_list
alloc_list: alloc_item (COMMA alloc_item)*
alloc_item: VAR LPAR args RPAR

deallocate_stmt: "DEALLOCATE" dealloc_list
dealloc_list: dealloc_item (COMMA dealloc_item)*
dealloc_item: VAR LPAR STAR RPAR

// ---- FOR/NEXT ----
for_stmt: "FOR" VAR EQ expr "TO" expr ("STEP" expr)?
next_stmt: "NEXT" VAR
EQ: "="

// ---- SELECT/CASE/END SELECT ----
select_stmt: "SELECT" expr
case_stmt: "CASE" case_vals
case_vals: "ELSE" | case_list
case_list: expr (COMMA expr)*
end_select_stmt: END_KW SELECT_KW
END_KW: "END"
SELECT_KW: "SELECT"

// ---- IF ----
if_then_inline: "IF" cond "THEN" inline_tail
inline_tail: non_nl_token+
non_nl_token: STRING
            | NUMBER
            | VAR
            | DEVICE
            | NAME
            | LPAR
            | RPAR
            | COMMA
            | PLUS | MINUS | MUL | DIV | POW
            | LT | LE | GT | GE | NE | EQ
            | SEMI | COLON

if_block_hdr: "IF" cond "THEN"
else_stmt: "ELSE"
end_if_stmt: END_KW IF_KW
IF_KW: "IF"

// ---- ERROR HANDLING ----
on_error_goto: "ON" "ERROR" "GOTO" (LABEL | NUMBER)
off_error: "OFF" "ERROR"

// ---- MASS STORAGE ----
mass_storage: "MASS" "STORAGE" "IS" STRING

// ---- ASSIGN/OUTPUT/CREATE/PURGE ----
assign_device_stmt: "ASSIGN" DEVICE "TO" (VAR | STAR) (SEMI assign_opts)?
assign_opts: ("FORMAT" "ON")? (COMMA "EOL" call_like)? (COMMA "APPEND")?
DEVICE: /@[A-Za-z][A-Za-z0-9_]*/
SEMI: ";"
COLON: ":"

output_stmt: "OUTPUT" DEVICE SEMI output_tail
output_tail: output_item ((COMMA | SEMI) output_item)*
output_item: STRING | expr

create_stmt: "CREATE" VAR COMMA NUMBER
purge_stmt: "PURGE" VAR

// ---- CALL / ASSIGNMENT ----
call_stmt: "CALL" call_like
call_like: VAR LPAR [args] RPAR
args: expr (COMMA expr)*

assignment: VAR LPAR args RPAR EQ expr   -> assign_idx
          | VAR EQ expr                  -> assign_var

// ---- COMMENT / EMPTY ----
comment_stmt: "!" /[^\n\r]+/  -> comment_text
            | "!"             -> comment_bang
empty_stmt:                     -> empty

// ---- COND / EXPR ----
cond: expr compop expr
compop: GE | LE | NE | GT | LT | EQ
GE: ">="  LE: "<="  NE: "<>"
GT: ">"   LT: "<"

?expr: expr PLUS term   -> add
     | expr MINUS term  -> sub
     | term
?term: term MUL pow     -> mul
     | term DIV pow     -> div
     | pow
?pow: unary
     | pow POW unary    -> pow
PLUS: "+" MINUS: "-" MUL: "*" DIV: "/"
POW: "^"

?unary: MINUS unary     -> neg
      | PLUS unary      -> pos
      | primary

?primary: NUMBER        -> number
        | STRING        -> string
        | call_like
        | VAR           -> var
        | LPAR expr RPAR

// ---- TOKENS ----
VAR: /[A-Za-z_][A-Za-z0-9_]*\$?/
NAME: /[A-Za-z_][A-Za-z0-9_]*/
STRING: ESCAPED_STRING

%import common.NUMBER
%import common.ESCAPED_STRING
%import common.NEWLINE
%import common.WS_INLINE
%ignore WS_INLINE
"""

# -----------------------------
# 3) Transformer (튜플 기반 AST)
# -----------------------------
@v_args(inline=True)
class BasicTransformer(Transformer):
    def start(self, *lines):
        return list(lines)

    def line(self, num, stmt):
        ln = int(num.value) if isinstance(num, Token) else int(num)
        return (ln, stmt)

    # DEF FN
    def def_fn(self, _def, name, _lp, args_or_none=None, _rp=None):
        fn = str(name)
        params = []
        if args_or_none:
            if isinstance(args_or_none, list):
                params = args_or_none
            else:
                params = [args_or_none]
        return ('DEF_FN', fn, params)

    def def_args(self, *items):
        flat = []
        for it in items:
            if isinstance(it, list):
                flat.extend(it)
            else:
                flat.append(it)
        return flat

    def def_item(self, x):
        return x

    def typed_group(self, typ, first_var, *rest_vars):
        tname = str(typ)
        out = [('PARAM', str(first_var), tname)]
        for v in rest_vars:
            out.append(('PARAM', str(v), tname))
        return out

    # COM
    def com_stmt(self, *_): return ('COM',)

    # DECL
    def integer_decl(self, *varlist):
        if len(varlist) == 1 and isinstance(varlist[0], list):
            return ('DECL_VARS', 'INTEGER', varlist[0])
        return ('DECL_VARS', 'INTEGER', list(varlist))
    def var_list(self, *vars_): return [str(v) for v in vars_]

    # ALLOC/DEALLOC
    def allocate_stmt(self, *parts):
        # 형태 단순화
        tname = None
        items = []
        for p in parts:
            if isinstance(p, Token):
                continue
            if isinstance(p, Tree) and p.data == 'alloc_list':
                items = p.children[0] if isinstance(p.children[0], list) else p.children
            elif isinstance(p, str):
                tname = p
            elif isinstance(p, list):
                items = p
            else:
                try:
                    tname = str(p)
                except:
                    pass
        return ('ALLOC', tname, items)

    def alloc_list(self, *items): return list(items)
    def alloc_item(self, name, _lp, args, _rp): return ('ALLOC_ITEM', str(name), args)
    def deallocate_stmt(self, items): return ('DEALLOC', items)
    def dealloc_list(self, *items): return list(items)
    def dealloc_item(self, name, _lp, _star, _rp): return ('DEALLOC_ITEM', str(name), '*')

    # FOR/NEXT
    def for_stmt(self, _for, var, _eq, e1, _to, e2, step_part=None):
        step = None
        if step_part and isinstance(step_part, tuple) and step_part[0] == 'STEP':
            step = step_part[1]
        return ('FOR', str(var), e1, e2, step)
    def next_stmt(self, _next, var): return ('NEXT', str(var))

    # SELECT/CASE
    def select_stmt(self, _sel, expr): return ('SELECT', expr)
    def case_stmt(self, _case, vals): return ('CASE', vals)
    def case_vals(self, x): return x
    def case_list(self, *xs): return list(xs)
    def end_select_stmt(self, *_): return ('END_SELECT',)

    # IF
    def if_then_inline(self, _if, cond, _then, tail): return ('IF_THEN_INLINE', cond, tail)
    def inline_tail(self, *tokens):
        vals = []
        for t in tokens:
            vals.append(t.value if isinstance(t, Token) else str(t))
        return vals
    def if_block_hdr(self, _if, cond, _then): return ('IF_THEN_HDR', cond)
    def else_stmt(self, *_): return ('ELSE',)
    def end_if_stmt(self, *_): return ('END_IF',)

    # ERROR
    def on_error_goto(self, *_):
        target = _[-1]
        if isinstance(target, Token) and target.type == 'NUMBER':
            target = int(target.value)
        else:
            target = str(target)
        return ('ON_ERROR_GOTO', target)
    def off_error(self, *_): return ('OFF_ERROR',)

    # MASS STORAGE
    def mass_storage(self, *_):
        s = _[-1]
        return ('MASS_STORAGE', s[1:-1] if isinstance(s, str) and s.startswith('"') else str(s))

    # ASSIGN/OUTPUT/CREATE/PURGE
    def assign_device_stmt(self, *_):
        dev = None; target = None
        for t in _:
            if isinstance(t, Token) and t.type == 'DEVICE':
                dev = t.value
            elif isinstance(t, Token) and (t.type == 'VAR' or t.type == 'STAR'):
                target = t.value
        return ('ASSIGN', dev, target)

    def output_stmt(self, _out, dev, _semi, tail):
        return ('OUTPUT', dev.value if isinstance(dev, Token) else str(dev), tail)
    def output_tail(self, *items): return list(items)
    def output_item(self, x): return x

    def create_stmt(self, _create, var, _comma, num):
        return ('CREATE', str(var), self._num(num))
    def purge_stmt(self, _purge, var):
        return ('PURGE', str(var))

    # CALL / ASSIGNMENT
    def call_stmt(self, _call, call_like):
        name, args = call_like[1], call_like[2]
        return ('CALL', name, args)
    def call_like(self, name, _lp=None, args=None, _rp=None):
        return ('CALL', str(name), (args or []))
    def args(self, *xs): return list(xs)
    def assign_idx(self, name, _lp, args, _rp, _eq, expr):
        return ('ASSIGN_IDX', str(name), args, expr)
    def assign_var(self, name, _eq, expr):
        return ('ASSIGN', str(name), expr)

    # COMMENT / EMPTY
    def comment_text(self, _bang, txt=None):
        s = txt.value if isinstance(txt, Token) else (txt or "")
        return ('COMMENT', s)
    def comment_bang(self, _bang=None): return ('COMMENT', '')
    def empty(self): return ('EMPTY',)

    # COND / EXPR
    def cond(self, a, op, b): return ('COND', str(op), a, b)
    def add(self, a, b): return ('ADD', a, b)
    def sub(self, a, b): return ('SUB', a, b)
    def mul(self, a, b): return ('MUL', a, b)
    def div(self, a, b): return ('DIV', a, b)
    def pow(self, a, b): return ('POW', a, b)
    def neg(self, x):    return ('NEG', x)
    def pos(self, x):    return ('POS', x)
    def number(self, tok):
        return self._num(tok)
    def string(self, tok):
        s = tok.value
        return s[1:-1]
    def var(self, name):
        return ('VAR', str(name))

    # helpers
    def _num(self, tok):
        s = tok.value if isinstance(tok, Token) else str(tok)
        try:
            if any(c in s for c in ('.', 'E', 'e')):
                return float(s)
            return int(s)
        except:
            return float(s)

# -----------------------------
# 4) 블록 결합(2nd pass)
# -----------------------------
Stmt = Tuple[str, ...]
LineStmt = Tuple[int, Stmt]

def _tag(stmt: Any) -> str:
    return stmt[0] if isinstance(stmt, tuple) and stmt else str(type(stmt))

def _parse_seq(lines: List[LineStmt], i: int, stop_tags: set) -> Tuple[List[LineStmt], int, str]:
    out: List[LineStmt] = []
    n = len(lines)
    while i < n:
        ln, st = lines[i]
        tag = _tag(st)

        if tag in stop_tags:
            return out, i, tag

        if tag == 'IF_THEN_HDR':
            cond = st[1]
            then_body, j, stopper = _parse_seq(lines, i+1, {'ELSE', 'END_IF'})
            else_body: List[LineStmt] = []
            if stopper == 'ELSE':
                _, else_stmt = lines[j]
                assert _tag(else_stmt) == 'ELSE'
                else_body, k, stopper2 = _parse_seq(lines, j+1, {'END_IF'})
                assert stopper2 == 'END_IF', "IF block must end with END_IF"
                _, end_if_stmt = lines[k]
                assert _tag(end_if_stmt) == 'END_IF'
                i = k + 1
            else:
                _, end_if_stmt = lines[j]
                assert _tag(end_if_stmt) == 'END_IF'
                i = j + 1
            out.append((ln, ('IF_BLOCK', cond, then_body, else_body)))

        elif tag == 'SELECT':
            selector = st[1]
            cases = []
            else_body = []
            j = i + 1
            n2 = len(lines)
            current = j
            while current < n2:
                ln2, st2 = lines[current]
                tag2 = _tag(st2)
                if tag2 == 'END_SELECT':
                    current += 1
                    break
                elif tag2 == 'CASE':
                    vals = st2[1]
                    if vals == 'ELSE':
                        body, k, stopper = _parse_seq(lines, current+1, {'END_SELECT'})
                        assert stopper == 'END_SELECT'
                        else_body = body
                        current = k
                    else:
                        body, k, stopper = _parse_seq(lines, current+1, {'CASE', 'END_SELECT'})
                        cases.append((vals, body))
                        current = k
                else:
                    # SELECT 안의 비정형 행은 건너뜀 (보수적)
                    body, k, stopper = _parse_seq(lines, current, {'CASE', 'END_SELECT'})
                    current = k
            if current <= n2-1 and _tag(lines[current][1]) == 'END_SELECT':
                current += 1
            out.append((ln, ('SELECT_BLOCK', selector, cases, else_body)))
            i = current

        else:
            out.append((ln, st))
            i += 1

    return out, i, None

def combine_blocks(prog: List[LineStmt]) -> List[LineStmt]:
    combined, i, stopper = _parse_seq(prog, 0, stop_tags=set())
    assert stopper is None, "Top-level parse should not stop on a tag"
    return combined

# -----------------------------
# 5) 요약 출력 헬퍼
# -----------------------------
def summarize_top_level(prog: List[LineStmt]):
    cnt = Counter()
    for ln, st in prog:
        if isinstance(st, tuple):
            cnt[st[0]] += 1
        else:
            cnt[type(st).__name__] += 1
    focus = [
        'DEF_FN',
        'FOR','NEXT',
        'SELECT','CASE','END_SELECT',
        'IF_THEN_INLINE','IF_THEN_HDR','ELSE','END_IF',
        'ON_ERROR_GOTO','OFF_ERROR',
        'ASSIGN','ASSIGN_IDX','CALL',
        'OUTPUT','CREATE','PURGE',
        'DECL_VARS','ALLOC','DEALLOC',
        'COM','COMMENT','EMPTY',
        'IF_BLOCK','SELECT_BLOCK'
    ]
    print("\n---- Top-level statement counts ----")
    for k in focus:
        if cnt.get(k):
            print(f"{k:14s}: {cnt[k]}")
    print("------------------------------------")

def summarize_blocks(prog: List[LineStmt]):
    c = Counter()
    def walk(node):
        if isinstance(node, tuple):
            c[node[0]] += 1
            if node[0] == 'IF_BLOCK':
                _, _, then_body, else_body = node
                for ln, st in then_body: walk(st)
                for ln, st in else_body: walk(st)
            elif node[0] == 'SELECT_BLOCK':
                _, _, cases, else_body = node
                for vals, body in cases:
                    for ln, st in body: walk(st)
                for ln, st in else_body: walk(st)
        elif isinstance(node, list):
            for x in node: walk(x)
    for ln, st in prog: walk(st)

    print("\n---- Combined node counts (deep) ----")
    for k in ("IF_BLOCK", "SELECT_BLOCK", "CASE", "IF_THEN_INLINE", "ASSIGN", "CALL", "OUTPUT"):
        if c.get(k):
            print(f"{k:16s}: {c[k]}")
    print("-------------------------------------")

# -----------------------------
# 6) 메인: 파싱 → 변환 → 블록결합 → 요약
# -----------------------------
def main():
    parser = Lark(GRAMMAR, parser="lalr", start="start", lexer="basic")
    tree = parser.parse(BASIC_CODE)

    tr = BasicTransformer()
    prog = tr.transform(tree)              # [(lineno, stmt), ...]
    combined = combine_blocks(prog)        # IF/SELECT 블록 결합

    # 총 라인 수
    print("✅ Parse + Transform + Combine success.")
    print("Total lines parsed:", len(prog))

    # 상위 레벨 카운트
    summarize_top_level(prog)

    # 결합 후 딥 카운트
    summarize_blocks(combined)

    # 앞쪽 10줄 미리보기
    print("\n---- First 10 lines (lineno, AST head after combine?) ----")
    for ln, st in combined[:10]:
        head = st[0] if isinstance(st, tuple) else type(st).__name__
        print(f"Line {ln}: {head}")

if __name__ == "__main__":
    main()