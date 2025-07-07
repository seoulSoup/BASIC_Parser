from lark import Lark, Transformer, v_args

basic_grammar = r"""
    start: line+

    line: LINENUM statement

    LINENUM: /\d+/

    statement: let_stmt
             | print_stmt
             | if_stmt
             | goto_stmt
             | end_stmt

    let_stmt: "LET" VAR "=" expr
    print_stmt: "PRINT" print_args
    if_stmt: "IF" condition "THEN" statement
    goto_stmt: "GOTO" LINENUM
    end_stmt: "END"

    print_args: (STRING | VAR | ";" | ",")+

    condition: expr COMPOP expr
    COMPOP: ">" | "<" | "="

    expr: expr "+" term -> add
        | expr "-" term -> sub
        | term

    term: NUMBER -> number
        | VAR    -> variable
        | "(" expr ")"

    VAR: /[A-Z]+/
    STRING: ESCAPED_STRING

    %import common.ESCAPED_STRING
    %import common.NUMBER
    %import common.WS
    %ignore WS
"""

@v_args(inline=True)
class BasicTransformer(Transformer):
    def start(self, *lines):
        return list(lines)

    def line(self, linenum, stmt):
        return (int(linenum), stmt)

    def let_stmt(self, var, value):
        return ('LET', var, value)

    def print_stmt(self, *args):
        return ('PRINT', args)

    def if_stmt(self, cond, stmt):
        return ('IF', cond, stmt)

    def goto_stmt(self, linenum):
        return ('GOTO', int(str(linenum)))

    def end_stmt(self):
        return ('END',)

    def condition(self, left, op, right):
        return ('COND', op, left, right)

    def number(self, n): return int(n)
    def variable(self, name): return str(name)
    def add(self, a, b): return ('+', a, b)
    def sub(self, a, b): return ('-', a, b)
    def print_args(self, *args): return list(args)