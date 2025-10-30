from lark import Lark, Transformer, v_args, Tree, Token

# ---------- BASIC Grammar (수정 포함: comment_stmt 정규식 수정됨) ----------
basic_grammar = r"""
    start: line+

    line: LINENUM statement NEWLINE?

    LINENUM: /\d+/
    ENDIF: "ENDIF"
    ELSEIF: "ELSEIF"

    statement: elseif_stmt
             | else_stmt
             | if_stmt
             | let_stmt
             | assign_stmt
             | print_stmt
             | goto_stmt
             | return_stmt
             | end_stmt
             | comment_stmt

    if_stmt: "IF" condition "THEN" (NEWLINE block_lines ENDIF | statement)
    elseif_stmt: ELSEIF condition "THEN" (NEWLINE block_lines | statement)
    else_stmt: "ELSE" NEWLINE block_lines
    block_lines: line+

    let_stmt: "LET" VAR "=" expr
    assign_stmt: VAR "=" expr
    print_stmt: "PRINT" print_args
    goto_stmt: "GOTO" LINENUM
    return_stmt: "RETURN" expr?
    end_stmt: "FNEND"

    comment_stmt: /!\s?.*/

    print_args: (STRING | VAR | ";" | ",")+

    condition: expr COMPOP expr | "(" expr COMPOP expr ")"
    COMPOP: ">" | "<" | "=" | ">=" | "<="

    expr: expr "+" term -> add
        | expr "-" term -> sub
        | term
    term: term "*" factor -> mul
        | term "/" factor -> div
        | factor
    factor: NUMBER -> number
          | VAR -> variable
          | "(" expr ")"

    VAR: /[A-Z][A-Z0-9_]*/
    STRING: ESCAPED_STRING

    %import common.ESCAPED_STRING
    %import common.NUMBER
    %import common.WS_INLINE
    %import common.NEWLINE
    %ignore WS_INLINE
"""

# ✅ 파서 생성 시도
parser = Lark(basic_grammar, parser='lalr', propagate_positions=True)
