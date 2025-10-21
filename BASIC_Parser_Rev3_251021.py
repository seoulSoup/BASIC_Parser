import re
import sys

class BasicInterpreter:
    def __init__(self, code: str):
        self.lines = self._parse_lines(code)
        self.line_map = {line['num']: i for i, line in enumerate(self.lines)}
        self.vars = {}
        self.pc = 0  # program counter
        self.loop_stack = []     # for FOR/NEXT, LOOP/END LOOP
        self.select_stack = []   # for SELECT CASE blocks

    # -----------------------------
    # 1️⃣ BASIC 코드 파싱
    # -----------------------------
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

    # -----------------------------
    # 2️⃣ 프로그램 실행
    # -----------------------------
    def run(self):
        while self.pc < len(self.lines):
            stmt = self.lines[self.pc]['stmt']
            self.execute(stmt)
            self.pc += 1

    # -----------------------------
    # 3️⃣ 명령 실행
    # -----------------------------
    def execute(self, stmt):
        # LET var = expr
        if stmt.startswith("LET "):
            var, expr = stmt[4:].split("=", 1)
            self.vars[var.strip()] = self._eval(expr.strip())

        # PRINT expr
        elif stmt.startswith("PRINT "):
            val = self._eval(stmt[6:].strip())
            print(val)

        # IF cond THEN stmt
        elif stmt.startswith("IF "):
            m = re.match(r"IF (.+) THEN (.+)", stmt)
            if not m:
                raise SyntaxError(f"Invalid IF syntax: {stmt}")
            cond, then_stmt = m.groups()
            if self._eval(cond):
                self.execute(then_stmt)

        # GOTO line
        elif stmt.startswith("GOTO "):
            target = int(stmt[5:].strip())
            self.pc = self.line_map[target] - 1  # -1 for pc += 1 after execute

        # FOR ... TO ...
        elif stmt.startswith("FOR "):
            m = re.match(r"FOR (\w+) = ([^ ]+) TO ([^ ]+)", stmt)
            if not m:
                raise SyntaxError(f"Invalid FOR syntax: {stmt}")
            var, start, end = m.groups()
            start_val, end_val = self._eval(start), self._eval(end)
            self.vars[var] = start_val
            self.loop_stack.append({
                'type': 'FOR',
                'var': var,
                'end': end_val,
                'start_pc': self.pc
            })

        # NEXT var
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

        # LOOP
        elif stmt == "LOOP":
            self.loop_stack.append({'type': 'LOOP', 'start_pc': self.pc})

        # EXIT IF ...
        elif stmt.startswith("EXIT IF "):
            cond = stmt[8:].strip()
            if self._eval(cond):
                # jump to corresponding END LOOP
                for i in range(self.pc + 1, len(self.lines)):
                    if self.lines[i]['stmt'] == "END LOOP":
                        self.pc = i
                        self.loop_stack.pop()
                        break

        # END LOOP
        elif stmt == "END LOOP":
            if not self.loop_stack or self.loop_stack[-1]['type'] != 'LOOP':
                raise SyntaxError("END LOOP without LOOP")
            loop = self.loop_stack[-1]
            self.pc = loop['start_pc'] - 1

        # SELECT CASE expr
        elif stmt.startswith("SELECT CASE "):
            value = self._eval(stmt[12:].strip())
            self.select_stack.append({'value': value, 'matched': False, 'active': True})

        # CASE value
        elif stmt.startswith("CASE "):
            val = self._eval(stmt[5:].strip())
            select = self.select_stack[-1]
            select['matched'] = (select['value'] == val)
            select['active'] = select['matched']

        # END SELECT
        elif stmt == "END SELECT":
            if not self.select_stack:
                raise SyntaxError("END SELECT without SELECT CASE")
            self.select_stack.pop()

        # END
        elif stmt == "END":
            sys.exit(0)

        # SELECT 블록 내 문장
        elif self.select_stack:
            sel = self.select_stack[-1]
            if sel['active']:
                self.execute(stmt)

        # 무시 가능한 빈문
        elif stmt.strip() == "":
            return

        else:
            raise NotImplementedError(f"Unknown statement: {stmt}")

    # -----------------------------
    # 4️⃣ 수식 계산기
    # -----------------------------
    def _eval(self, expr):
        try:
            return eval(expr, {}, self.vars)
        except Exception as e:
            raise RuntimeError(f"Eval error: '{expr}' ({e})")

# -----------------------------
# 테스트용 BASIC 코드
# -----------------------------
if __name__ == "__main__":
    basic_code = """
10 LET A = 3
20 FOR I = 1 TO 3
30 PRINT I
40 NEXT I
50 SELECT CASE A
60 CASE 1
70 PRINT 100
80 CASE 3
90 PRINT 300
100 END SELECT
110 LET X = 0
120 LOOP
130 LET X = X + 1
140 PRINT X
150 EXIT IF X > 2
160 END LOOP
170 END
"""
    BasicInterpreter(basic_code).run()
    
    
    .