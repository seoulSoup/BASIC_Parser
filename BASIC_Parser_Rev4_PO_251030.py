# pip install lark
from lark import Lark, Tree, Token
from lark.exceptions import UnexpectedInput

BASIC_CODE = r"""(여기에 네 예시 BASIC 전체 붙이기)"""

GRAMMAR = r"""
start: line+
line: NUMBER statement NEWLINE?      -> line

statement: labeled_stmt
         | plain_stmt

labeled_stmt: LABEL ":" plain_stmt
// ★ LABEL과 VAR/NAME 충돌 줄이기 위해 LABEL은 statement 맨 앞에서만 등장
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
          | return_stmt               // ★ 추가
          | fnend_stmt                // ★ 추가
          | call_like_stmt            // ★ 추가: CALL 없이 함수/배열 호출을 문장으로
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
// ★ NAME을 슬래시 사이 “그냥 문자열”로 정의해서 VAR와 충돌 제거
com_stmt: "COM" "/" NAME "/" com_decls (COMMA com_decls)*
NAME: /[^/\n\r]+/
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
non_nl_token: STRING | NUMBER | VAR | DEVICE | LPAR | RPAR | COMMA
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
assign_device_stmt: "ASSIGN" DEVICE "TO" assign_target (SEMI assign_opts)?
// ★ '*'가 산술 MUL로 토큰화돼도 허용되도록 literal '*'도 수용
assign_target: VAR | STAR | MUL
assign_opts: ("FORMAT" "ON")? (COMMA "EOL" call_like)? (COMMA "APPEND")?
DEVICE: /@[A-Za-z][A-Za-z0-9_]*/
SEMI: ";"
COLON: ":"

output_stmt: "OUTPUT" DEVICE SEMI output_tail
output_tail: output_item ((COMMA | SEMI) output_item)*
output_item: STRING | expr

create_stmt: "CREATE" VAR COMMA NUMBER
purge_stmt: "PURGE" VAR

// ---- RETURN / FNEND (추가)
return_stmt: "RETURN" expr
fnend_stmt: "FNEND"

// ---- CALL / 호출문장 / ASSIGNMENT ----
call_stmt: "CALL" call_like
// ★ CALL 없이도 호출을 문장으로 허용
call_like_stmt: call_like

// ★ 배열/함수 “적용” 형태는 통일해서 expr에서 사용 가능해야 함
call_like: VAR LPAR [args] RPAR
args: expr (COMMA expr)*

assignment: VAR LPAR args RPAR EQ expr   -> assign_idx
          | VAR EQ expr                  -> assign_var

// ---- COMMENT / EMPTY ----
comment_stmt: "!" /[^\n\r]*/  -> comment_text   // ★ 빈 주석 허용
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
        | call_like                         // ★ 배열/함수 적용 둘 다 허용
        | VAR           -> var
        | LPAR expr RPAR

// ---- TOKENS ----
VAR: /[A-Za-z_][A-Za-z0-9_]*\$?/
STRING: ESCAPED_STRING

%import common.NUMBER
%import common.ESCAPED_STRING
%import common.NEWLINE
%import common.WS_INLINE
%ignore WS_INLINE
"""

def _lex_window(text, pos, width=120):
    a = max(0, pos-width//2); b = min(len(text), pos+width//2)
    caret = ' ' * (pos-a) + '^'
    return text[a:b] + "\n" + caret

def run_parse():
    parser = Lark(GRAMMAR, parser="lalr", start="start", lexer="basic")
    try:
        tree = parser.parse(BASIC_CODE)
        print("✅ PARSE OK")
        # 간단 요약: line 노드 개수 / 상위 노드 종류
        line_nodes = [n for n in tree.children if isinstance(n, Tree) and n.data=="line"]
        print("Lines:", len(line_nodes))
        # 상위 statement 태그 카운트
        from collections import Counter
        cnt = Counter()
        for ln in line_nodes:
            stmt = ln.children[1]
            if isinstance(stmt, Tree):
                cnt[stmt.data]+=1
        print("Top statements:", dict(cnt))
    except UnexpectedInput as e:
        print("❌ PARSE FAIL")
        print(type(e).__name__, str(e))
        if hasattr(e, 'pos_in_stream'):
            print("\n--- around offending location ---")
            print(_lex_window(BASIC_CODE, e.pos_in_stream))
        # 토큰 스트림 일부를 보고 싶다면 아래 주석 해제
        # print("\nTokens near error:")
        # for t in parser.lex(BASIC_CODE[e.pos_in_stream-40:e.pos_in_stream+40]):
        #     print(t)

if __name__ == "__main__":
    run_parse()