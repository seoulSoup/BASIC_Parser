class BasicInterpreter:
    def __init__(self, program):
        self.env = {}
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
        return 0

    def eval_cond(self, cond):
        _, op, left, right = cond
        l = self.eval_expr(left)
        r = self.eval_expr(right)
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
                self.env[var] = self.eval_expr(val)

            elif action == 'PRINT':
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

            elif action == 'IF':
                _, cond, then_stmt = stmt
                if self.eval_cond(cond):
                    jump = self.execute_stmt(then_stmt)
                    if jump is not None:
                        i = jump
                        continue

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