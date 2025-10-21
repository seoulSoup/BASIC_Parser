import re

class BasicInterpreter:
    def __init__(self, code: str):
        self.lines = self._parse_lines(code)
        self.line_map = {line['num']: i for i, line in enumerate(self.lines)}
        self.vars = {}
        self.pc = 0
        self.loop_stack = []
        self.select_stack = []
        self.halt = False

    def _parse_lines(self, code):
        lines = []
        for raw in code.strip().splitlines():
            if not raw.strip():
                continue
            m = re.match(r'(\d+)\s+(.*)', raw.strip())
            if not m:
                raise SyntaxError(f"Invalid line format: {raw}")
            num, stmt = int(m.group(1)), m.group(2).strip()
            lines.append({'num': num, 'stmt': stmt})
        return lines

    def run(self):
        while self.pc < len(self.lines) and not self.halt:
            stmt = self.lines[self.pc]['stmt']
            self.execute(stmt)
            self.pc += 1

    def execute(self, stmt):
        # --------------------------------------------------
        # 1️⃣ 제어 블록 밖의 일반 문장 처리
        # --------------------------------------------------
        if stmt.startswith("LET "):
            var, expr = stmt[4:].split("=", 1)
            self.vars[var.strip()] = self._eval(expr.strip())

        elif stmt.startswith("PRINT "):
            print(self._eval(stmt[6:].strip()))

        elif stmt.startswith("IF "):
            m = re.match(r"IF (.+) THEN (.+)", stmt)
            if not m:
                raise SyntaxError(f"Invalid IF: {stmt}")
            cond, then_stmt = m.groups()
            if self._eval(cond):
                self.execute(then_stmt)

        elif stmt.startswith("GOTO "):
            target = int(stmt[5:].strip())
            self.pc = self.line_map[target] - 1

        elif stmt.startswith("FOR "):
            m = re.match(r"FOR (\w+) = ([^ ]+) TO ([^ ]+)", stmt)
            if not m:
                raise SyntaxError(f"Invalid FOR: {stmt}")
            var, start, end = m.groups()
            self.vars[var] = self._eval(start)
            self.loop_stack.append({
                'type': 'FOR',
                'var': var,
                'end': self._eval(end),
                'start_pc': self.pc
            })

        elif stmt.startswith("NEXT "):
            var = stmt[5:].strip()
            loop = self.loop_stack[-1]
            if loop['type'] != 'FOR':
                raise SyntaxError("NEXT without FOR")
            self.vars[var] += 1
            if self.vars[var] <= loop['end']:
                self.pc = loop['start_pc']
            else:
                self.loop_stack.pop()

        elif stmt == "LOOP":
            self.loop_stack.append({'type': 'LOOP', 'start_pc': self.pc})

        elif stmt.startswith("EXIT IF "):
            cond = stmt[8:].strip()
            if self._eval(cond):
                for i in range(self.pc + 1, len(self.lines)):
                    if self.lines[i]['stmt'] == "END LOOP":
                        self.pc = i
                        self.loop_stack.pop()
                        break

        elif stmt == "END LOOP":
            if not self.loop_stack or self.loop_stack[-1]['type'] != 'LOOP':
                raise SyntaxError("END LOOP without LOOP")
            loop = self.loop_stack[-1]
            self.pc = loop['start_pc'] - 1

        elif stmt.startswith("SELECT CASE "):
            value = self._eval(stmt[12:].strip())
            self.select_stack.append({
                'value': value,
                'matched': False,
                'active': False,
                'finished': False
            })

        elif stmt.startswith("CASE "):
            val = self._eval(stmt[5:].strip())
            select = self.select_stack[-1]

            # 이미 매칭된 CASE가 있으면 이후는 모두 비활성화
            if select['matched']:
                select['active'] = False
            else:
                # 첫 매칭된 CASE만 활성화
                if select['value'] == val:
                    select['matched'] = True
                    select['active'] = True
                else:
                    select['active'] = False

        elif stmt == "END SELECT":
            if not self.select_stack:
                raise SyntaxError("END SELECT without SELECT CASE")
            self.select_stack.pop()

        elif stmt == "END":
            self.halt = True

        # --------------------------------------------------
        # 2️⃣ SELECT CASE 블록 내부 실행 처리
        # --------------------------------------------------
        elif self.select_stack:
            current = self.select_stack[-1]
            # CASE가 활성화된 경우에만 실행
            if current['active']:
                self.execute(stmt)

        else:
            raise NotImplementedError(f"Unknown statement: {stmt}")

    def _eval(self, expr):
        try:
            return eval(expr, {}, self.vars)
        except Exception as e:
            raise RuntimeError(f"Eval error: '{expr}' ({e})")


# ---------------------- 테스트 코드 ----------------------
if __name__ == "__main__":
    basic_code = """
10 LET A = 3
20 SELECT CASE A
30 CASE 1
40 PRINT "A is 1"
50 CASE 2
60 PRINT "A is 2"
70 CASE 3
80 PRINT "A is 3"
90 CASE 4
100 PRINT "A is 4"
110 END SELECT
120 END
"""
    BasicInterpreter(basic_code).run()