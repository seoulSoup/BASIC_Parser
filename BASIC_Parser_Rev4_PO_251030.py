# -*- coding: utf-8 -*-
# pip install lark
from lark import Lark, Tree
from lark.exceptions import UnexpectedInput
from collections import Counter

# ----- 네가 준 전체 예시코드 붙이기 -----
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

# ----- Grammar (raw-string, 숫자토큰 재정의 포함) -----
GRAMMAR = r"""
start: line+
line: NUMBER statement NEWLINE?      -> line

// ----- statements -----
statement: labeled_stmt
         | plain_stmt

labeled_stmt: LABEL ":" plain_stmt
LABEL: /[A-Za-z_][A-Za-z0-9_]*/

// 순서 주의: CALL 없이 호출문 먼저 허용하면 애매해질 수 있으므로 아래 plain_stmt 내에 위치만 보장
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
          | return_stmt
          | fnend_stmt
          | call_like_stmt           // ★ CALL 없이도 호출문 허용
          | assignment
          | comment_stmt
          | empty_stmt

// ----- DEF FN -----
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

// ----- COM -----
SLASHNAME: /\/[^\/\r\n]+\//
com_stmt: "COM" SLASHNAME com_decls (COMMA com_decls)*
com_decls: TYPE (VAR | array_any)
array_any: VAR LPAR STAR RPAR
STAR: "*"

// ----- DECL -----
integer_decl: INTEGER_KW var_list
var_list: VAR (COMMA VAR)*

// ----- ALLOC / DEALLOC -----
allocate_stmt: "ALLOCATE" TYPE alloc_list
alloc_list: alloc_item (COMMA alloc_item)*
alloc_item: VAR LPAR args RPAR

deallocate_stmt: "DEALLOCATE" dealloc_list
dealloc_list: dealloc_item (COMMA dealloc_item)*
dealloc_item: VAR LPAR STAR RPAR

// ----- FOR/NEXT -----
for_stmt: "FOR" VAR EQ expr "TO" expr ("STEP" expr)?
next_stmt: "NEXT" VAR
EQ: "="

// ----- SELECT/CASE/END SELECT -----
select_stmt: "SELECT" expr
case_stmt: "CASE" case_vals
case_vals: "ELSE" | case_list
case_list: expr (COMMA expr)*
end_select_stmt: END_KW SELECT_KW
END_KW: "END"
SELECT_KW: "SELECT"

// ----- IF -----
if_then_inline: "IF" cond "THEN" inline_tail
inline_tail: non_nl_token+
non_nl_token: STRING | NUMBER | VAR | DEVICE | LPAR | RPAR | COMMA
            | PLUS | MINUS | MUL | DIV | POW
            | LT | LE | GT | GE | NE | EQ
            | SEMI | COLON

if_block_hdr: "IF" cond "THEN"
else_stmt: "ELSE"
end_if_stmt: END_KW IF_KW
IF_KW: "IF"

// ----- ERROR HANDLING -----
on_error_goto: "ON" "ERROR" "GOTO" (LABEL | NUMBER)
off_error: "OFF" "ERROR"

// ----- MASS STORAGE -----
mass_storage: "MASS" "STORAGE" "IS" STRING

// ----- ASSIGN/OUTPUT/CREATE/PURGE -----
assign_device_stmt: "ASSIGN" DEVICE "TO" assign_target (SEMI assign_opts)?
assign_target: VAR | STAR | MUL              // ★ '*'가 MUL로 와도 허용
assign_opts: ("FORMAT" "ON")? (COMMA "EOL" call_like)? (COMMA "APPEND")?
DEVICE: /@[A-Za-z][A-Za-z0-9_]*/
SEMI: ";"
COLON: ":"

output_stmt: "OUTPUT" DEVICE SEMI output_tail
output_tail: output_item ((COMMA | SEMI) output_item)*
output_item: STRING | expr

create_stmt: "CREATE" VAR COMMA NUMBER
purge_stmt: "PURGE" VAR

// ----- RETURN / FNEND -----
return_stmt: "RETURN" expr
fnend_stmt: "FNEND"

// ----- CALL / 호출문장 / ASSIGNMENT -----
call_stmt: "CALL" call_like
call_like_stmt: call_like
call_like: VAR LPAR [args] RPAR
args: expr (COMMA expr)*

assignment: VAR LPAR args RPAR EQ expr   -> assign_idx
          | VAR EQ expr                  -> assign_var

// ----- COMMENT / EMPTY -----
comment_stmt: "!" /[^\n\r]*/  -> comment_text
empty_stmt:                     -> empty

// ----- COND / EXPR -----
cond: expr compop expr
compop: GE | LE | NE | GT | LT | EQ
GE: ">="  LE: "<="  NE: "<>"
GT: ">"   LT: "<"

// 숫자: 1.E-2 같은 형식까지 허용
NUMBER: /-?\d+(?:\.\d*)?(?:[Ee][+\-]?\d+)?/

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

VAR: /[A-Za-z_][A-Za-z0-9_]*\$?/
STRING: ESCAPED_STRING

%import common.ESCAPED_STRING
%import common.NEWLINE
%import common.WS_INLINE
%ignore WS_INLINE
"""

def _lex_window(text, pos, width=120):
    a = max(0, pos-width//2); b = min(len(text), pos+width//2)
    caret = ' ' * (pos-a) + '^'
    return text[a:b] + "\n" + caret

def main():
    parser = Lark(GRAMMAR, parser="lalr", start="start", lexer="basic")
    try:
        tree = parser.parse(BASIC_CODE)
        print("✅ PARSE OK")
        # 라인/상위태그 요약
        line_nodes = [n for n in tree.children if isinstance(n, Tree) and n.data=="line"]
        print("Lines parsed:", len(line_nodes))
        cnt = Counter()
        for ln in line_nodes:
            stmt = ln.children[1]
            if isinstance(stmt, Tree): cnt[stmt.data]+=1
        print("Top-level statement counts:")
        for k,v in cnt.most_common():
            print(f"  {k:16s} {v}")
    except UnexpectedInput as e:
        print("❌ PARSE FAIL:", type(e).__name__)
        print(str(e))
        if hasattr(e, 'pos_in_stream'):
            print("\n--- around offending location ---")
            print(_lex_window(BASIC_CODE, e.pos_in_stream))

if __name__ == "__main__":
    main()