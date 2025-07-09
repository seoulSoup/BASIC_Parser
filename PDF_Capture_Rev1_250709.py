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
             | end_stmt

    if_stmt: "IF" condition "THEN" (NEWLINE block_lines ENDIF | statement)
    elseif_stmt: ELSEIF condition "THEN" (NEWLINE block_lines | statement)
    else_stmt: "ELSE" NEWLINE block_lines

    
    
    block_lines: line+
    
    let_stmt: "LET" VAR "=" expr
    assign_stmt: VAR "=" expr
    print_stmt: "PRINT" print_args
    goto_stmt: "GOTO" LINENUM
    end_stmt: "END"

    print_args: (STRING | VAR | ";" | ",")+

    condition: expr COMPOP expr | "(" expr COMPOP expr ")"
    COMPOP: ">" | "<" | "=" | ">=" | "<="

    expr: expr "+" term -> add
        | expr "-" term -> sub
        | term

    term: NUMBER -> number
        | VAR    -> variable
        | "(" expr ")"

    VAR: /[A-Z0-9]+/
    STRING: ESCAPED_STRING

    %import common.ESCAPED_STRING
    %import common.NUMBER
    %import common.WS_INLINE
    %ignore WS_INLINE
    %import common.NEWLINE
"""

@v_args(inline=True)
class BasicTransformer(Transformer):
    def start(self, *lines):
        return list(lines)

    def line(self, linenum, stmt):
        return (int(linenum), stmt)

    def let_stmt(self, var, value):
        return ('LET', var, value)

    def assign_stmt(self, var, value):
        return ('LET', var, value)

    def print_stmt(self, *args):
        return ('PRINT', args)

    def if_else_block(self, cond, then_lines, *rest):
        else_ifs = []
        else_block = []
        for part in rest:
            if isinstance(part, tuple) and part[0] == "ELSE IF":
                else_ifs.append(part[1:])
            elif isinstance(part, tuple) and part[0] == "ELSE":
                else_block = part[1]
        then_block = [stmt for _, stmt in then_lines]
        return ('IF_BLOCK_FULL', cond, then_block, else_ifs, else_block)
        # return ('IF_BLOCK_FULL', cond, else_ifs, else_block)

    def simple_if_block(self, cond, lines, _end, _if):
        stmts = [stmt for _, stmt in lines]
        return ('IF_BLOCK', cond, stmts)
        
    def if_then_stmt(self, cond, stmt):
        return ('IF_THEN', cond, stmt)
    def if_stmt(self, cond, *rest):
        if len(rest) == 1 and isinstance(rest[0], tuple):
            # 단일 IF THEN statement
            return ('IF_THEN', cond, rest[0])
        else:
            block_lines = [stmt for _, stmt in rest[1]]
            return ('IF_BLOCK', cond, block_lines)
        
    # def else_if_clause(self, cond, lines, _):
    #     print(cond)
    #     print(lines)
    #     print(_)
    #     stmts = [stmt for _, stmt in lines]
    #     return ('ELSE_IF', cond, stmts)
        
    # def if_block_stmt(self, cond, then_lines, *rest):
    #     else_ifs = []
    #     else_block = []

    #     for part in rest:
    #         if isinstance(part, tuple) and part[0] == "ELSE IF":
    #             else_ifs.append(part[1:])
    #         elif isinstance(part, tuple) and part[0] == "ELSE":
    #             else_block = part[1]
    #     then_block = [stmt for _, stmt in then_lines]
    #     return 'IF_BLOCK_FULL', cond, then_block, else_ifs, else_block

    # def else_clause(self, lines):
    #     stmts = [stmt for _, stmt in lines]
    #     return ('ELSE', stmts)

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
    def expr(self, val): return val
    def term(self, val): return val
    def print_args(self, *args): return list(args)
    
    def statement(self, stmt):
        return stmt
