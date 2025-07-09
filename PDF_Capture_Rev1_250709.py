import fitz
import re

basic_grammar = r"""
    start: line+

    line: LINENUM statement NEWLINE?

    LINENUM: /\d+/

    END_IF: "END IF"
    ELSE_IF: "ELSE IF"

    statement: if_block_stmt
             | if_then_stmt
             | let_stmt
             | assign_stmt
             | print_stmt
             | goto_stmt
             | END_IF
             | ELSE_IF
             
             
             

    
    else_if_clause: ELSE_IF condition "THEN" block_lines 
    else_clause: "ELSE" NEWLINE block_lines
    if_block_stmt: if_else_block | simple_if_block
    if_else_block: "IF" condition "THEN" NEWLINE block_lines (else_if_clause)* (else_clause)? END_IF
    simple_if_block: "IF" condition "THEN" block_lines END_IF
    if_then_stmt: "IF" condition "THEN" statement

    
    
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
        print("If Else")
        else_ifs = []
        else_block = []
        for part in rest:
            print(part)
            if isinstance(part, tuple) and part[0] == "ELSE IF":
                else_ifs.append(part[1:])
            elif isinstance(part, tuple) and part[0] == "ELSE":
                else_block = part[1]
        then_block = [stmt for _, stmt in then_lines]
        return ('IF_BLOCK_FULL', cond, then_block, else_ifs, else_block)
        # return ('IF_BLOCK_FULL', cond, else_ifs, else_block)

    def simple_if_block(self, cond, lines, _end, _if):
        print("Simple")
        stmts = [stmt for _, stmt in lines]
        return ('IF_BLOCK', cond, stmts)
        
    def if_then_stmt(self, cond, stmt):
        print("If Then")
        return ('IF_THEN', cond, stmt)
        
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

class BasicInterpreter:
    def __init__(self, program):
        self.env = {}
        for x in program:
            print(type(x))
            print(x)
        self.program = dict(sorted(program))  # {linenum: stmt}
        self.linenums = list(self.program.keys())
        self.line_index = {ln: i for i, ln in enumerate(self.linenums)}  # linenum → index

    def eval_expr(self, expr):
        if isinstance(expr, int):
            return expr
        elif isinstance(expr, str):
            return self.env.get(expr, 0)
        elif isinstance(expr, tuple):
            op = expr[0]
            if op == '+':
                return self.eval_expr(expr[1]) + self.eval_expr(expr[2])
            elif op == '-':
                return self.eval_expr(expr[1]) - self.eval_expr(expr[2])
        elif isinstance(expr, Tree):
            return self.eval_expr(expr)
        return 0

    def eval_cond(self, cond):
        _, op, left, right = cond
        # print(left, right)
        l = self.eval_expr(left)
        r = self.eval_expr(right)
        # print(l, r)
        if op == '>': return l > r
        if op == '<': return l < r
        if op == '=': return l == r

    def run(self):
        i = 0
        while i < len(self.linenums):
            linenum = self.linenums[i]
            stmt = self.program[linenum]
            # GOTO 등을 위해 index를 미리 저장
            i_next = i + 1
            action = stmt[0]

            if action == 'LET':
                _, var, val = stmt
                self.env[var.value] = self.eval_expr(val)

            elif action == 'PRINT':
                _, args = stmt
                output = ''
                for item in args[0]:
                    if item.value == ';':
                        continue
                    elif isinstance(item.value, str) and item.startswith('"'):
                        output += item.value.strip('"')
                    else:
                        output += str(self.eval_expr(item.value))
                print(output)
            # ('IF_BLOCK_FULL', cond, else_ifs, else_block)
            elif action == 'IF_BLOCK_FULL':
                cond, else_ifs, then_block, else_block = stmt[1:]
                if self.eval_cond(cond):
                    for s in then_block:
                        self.execute_stmt(s)
                else:
                    executed = False
                    for ei_cond, ei_block in else_ifs:
                        if self.eval_cond(ei_cond):
                            for s in ei_block:
                                self.execute_stmt(s)
                            executed = True
                            break
                if not executed and else_block:
                    for s in else_block:
                        self.execute_stmt(s)


            elif action == 'IF_THEN':
                _, cond, then_stmt = stmt
                if self.eval_cond(cond):
                    jump = self.execute_stmt(then_stmt)
                    if jump is not None:
                        i = jump
                        continue
        
            elif action == 'IF_BLOCK':
                cond, block = stmt[1], stmt[2]
                if self.eval_cond(cond):
                    for s in block:
                        self.execute_stmt(s)

            
            elif action == 'GOTO':
                _, target = stmt
                if target in self.line_index:
                    i = self.line_index[target]
                    continue
                else:
                    raise RuntimeError(f"GOTO to undefined line: {target}")

            elif action == 'END':
                break

            i = i_next

    def execute_stmt(self, stmt):
        """조건문의 THEN에 대응하는 단일 실행"""
        # print(stmt)
        if stmt[0] == 'GOTO':
            _, target = stmt
            if target in self.line_index:
                return self.line_index[target]
            else:
                raise RuntimeError(f"GOTO to undefined line: {target}")
        elif stmt[0] == 'LET':
            _, var, val = stmt
            self.env[var] = self.eval_expr(val)
        elif stmt[0] == 'PRINT':
            _, args = stmt
            output = ''
            for item in args:
                if item == ';':
                    continue
                elif isinstance(item, str) and item.startswith('"'):
                    output += item.strip('"')
                else:
                    output += str(self.eval_expr(item))
            print(output)

code = """10 X=1
            20 PRINT "Start X="; X
            30 X = X+1
            40 IF X<5 THEN GOTO 20
            50 PRINT "End X="; X
            51 IF X>6 THEN
            52 PRINT "6"
            53 END IF
            60 IF X=5 THEN
            70 PRINT "A"
            80 PRINT "B"
            90 ELSE IF X>5 THEN
            100 PRINT "BIG"
            110 ELSE
            120 PRINT "SMALL"
            130 END IF
            140 END"""

parser = Lark(basic_grammar)
for tok in parser.lex(code):
    print(tok)
tree = parser.parse(code)
program = BasicTransformer().transform(tree)

interpreter = BasicInterpreter(program)
interpreter.run()
