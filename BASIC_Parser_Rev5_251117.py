# -*- coding: utf-8 -*-
# pip install lark


# ----- 네가 준 전체 예시코드 붙이기 -----


# ----- Grammar (raw-string, 숫자토큰 재정의 포함) -----
GRAMMAR = r"""
start: line+
line: NUMBER statement NEWLINE?      -> line

// ----- statements -----
statement: labeled_stmt
         | plain_stmt

# labeled_stmt: LABEL ":" plain_stmt
# LABEL: /[A-Za-z_][A-Za-z0-9_]*/
labeled_stmt: VAR COLON plain_stmt

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
          | call_stmt
          | call_like_stmt           // ★ CALL 없이도 호출문 허용
          | bare_call_stmt
          | assignment
        #   | comment_stmt
          | empty_stmt

// ----- DEF FN -----
def_fn: DEF_KW VAR LPAR [def_args] RPAR
DEF_KW: "DEF"
LPAR: "("
RPAR: ")"

def_args: def_item (COMMA def_item)*
def_item: typed_group | VAR
typed_group: type_kw (VAR | array_any)# (COMMA VAR)*

# TYPE: INTEGER_KW | REAL_KW
INTEGER_KW: "INTEGER"
REAL_KW: "REAL"
COMMA: ","
type_kw: INTEGER_KW | REAL_KW

// ----- COM -----
SLASHNAME: /\/[^\/\r\n]+\//
com_stmt: "COM" SLASHNAME com_decls (COMMA com_decls)*
# com_decls: type_kw (VAR | array_any)
com_decls: typed_group | array_any | VAR
# array_any: VAR LPAR STAR RPAR
array_any: VAR LPAR MUL RPAR
# STAR: "*"

// ----- DECL -----
integer_decl: INTEGER_KW var_list
var_list: VAR (COMMA VAR)*

// ----- ALLOC / DEALLOC -----
allocate_stmt: "ALLOCATE" type_kw alloc_list
alloc_list: alloc_item (COMMA alloc_item)*
alloc_item: VAR LPAR args RPAR

deallocate_stmt: "DEALLOCATE" dealloc_list
dealloc_list: dealloc_item (COMMA dealloc_item)*
# dealloc_item: VAR LPAR STAR RPAR
dealloc_item: VAR LPAR MUL RPAR

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
            | "CALL"

if_block_hdr: "IF" cond "THEN"
else_stmt: "ELSE"
end_if_stmt: END_KW IF_KW
IF_KW: "IF"

// ----- ERROR HANDLING -----
on_error_goto: "ON" "ERROR" "GOTO" (VAR | NUMBER)
off_error: "OFF" "ERROR"

// ----- MASS STORAGE -----
mass_storage: "MASS" "STORAGE" "IS" STRING

// ----- ASSIGN/OUTPUT/CREATE/PURGE -----
assign_device_stmt: "ASSIGN" DEVICE "TO" assign_target (SEMI assign_opts)?
# assign_target: VAR | STAR | MUL              // ★ '*'가 MUL로 와도 허용
assign_target: VAR | MUL              // ★ '*'가 MUL로 와도 허용
assign_opts: ("FORMAT" "ON")? (COMMA "EOL" call_like)? (COMMA "APPEND")?
DEVICE: /@[A-Za-z][A-Za-z0-9_]*/
SEMI: ";"
COLON: ":"

output_stmt: "OUTPUT" DEVICE SEMI output_tail
# output_tail: output_item ((COMMA | SEMI) output_item)*
# output_item: STRING | expr
output_tail: expr ((COMMA | SEMI) expr)*

create_stmt: "CREATE" VAR COMMA NUMBER
purge_stmt: "PURGE" VAR

// ----- RETURN / FNEND -----
return_stmt: "RETURN" expr
fnend_stmt: "FNEND"

// ----- CALL / 호출문장 / ASSIGNMENT -----
# call_stmt: "CALL" call_like
call_stmt: "CALL" VAR (LPAR [args] RPAR)?
call_like_stmt: call_like
call_like: VAR LPAR [args] RPAR
# call_like: VAR LPAR [args] RPAR | tis_command
# tis_command: "Connect" | "Disable_port"
args: expr (COMMA expr)*

bare_call_stmt: VAR

assignment: VAR LPAR args RPAR EQ expr   -> assign_idx
          | VAR EQ expr                  -> assign_var

// ----- COMMENT / EMPTY -----
# comment_stmt: "!" /[^\n\r]*/  | "!" -> comment_text
# COMMENT_TEXT: /[^\n\r]+/
# comment_stmt: "!"COMMENT_TEXT | "!"
empty_stmt:                     -> empty

// ----- COND / EXPR -----
# cond: expr compop expr
cond: bool_expr
?bool_expr: bool_expr "OR" bool_term -> or
            | bool_expr "AND" bool_term -> and
            | bool_term
?bool_term: rel_expr
?rel_expr: expr compop expr | expr
compop: GE | LE | NE | GT | LT | EQ
GE: ">="  
LE: "<="  
NE: "<>"
GT: ">"   
LT: "<"

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
PLUS: "+" 
MINUS: "-" 
MUL: "*" 
DIV: "/"
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
COMMENT: /![^\n\r]*/


%import common.ESCAPED_STRING
%import common.NEWLINE
%import common.WS_INLINE
%ignore WS_INLINE
%ignore COMMENT
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
