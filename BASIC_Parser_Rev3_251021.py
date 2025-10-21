import math
import random
import sys
import re
from collections import deque, defaultdict

# =========================
# Tokenizer (hand-rolled)
# =========================
KEYWORDS = {
    'LET','IF','THEN','ELSE','FOR','TO','STEP','NEXT','GOTO','GOSUB','RETURN','END','STOP',
    'PRINT','INPUT','DIM','DATA','READ','RESTORE',
    'AND','OR','NOT',
    'SELECT','CASE','END','SELECT','CASE','ELSE','END','SELECT','END','SELECT',  # dup safety
    'END','SELECT','END','IF',
    'WHILE','END','LOOP','REPEAT','UNTIL',
    'ON',
    'SUB','SUBEND','DEF','FN','FNEND',
}

# normalize combined keywords for parser convenience
COMBINED = {
    ('END','IF'): 'END_IF',
    ('END','SELECT'): 'END_SELECT',
    ('END','WHILE'): 'END_WHILE',
    ('DEF','FN'): 'DEF_FN',
    ('FNEND',): 'FNEND',
    ('SUBEND',): 'SUBEND',
}

BUILTINS = {
    'SIN': math.sin, 'COS': math.cos, 'TAN': math.tan,
    'ABS': abs, 'LOG': math.log, 'EXP': math.exp, 'INT': lambda x: int(math.floor(x)),
    'RND': lambda: random.random(),
}

class Token:
    def __init__(self, typ, val, pos):
        self.type = typ  # 'NUM','STR','ID','KW','OP','SEP','EOL','LINENUM'
        self.val = val
        self.pos = pos
    def __repr__(self):
        return f"Token({self.type},{self.val})"

def tokenize_line(line):
    s = line.strip()
    i = 0
    tokens = []
    def peek(): return s[i] if i < len(s) else ''
    def advance(): 
        nonlocal i
        ch = s[i]; i += 1; return ch

    # first token must be line number
    m = re.match(r'\s*(\d+)\s*(.*)$', s)
    if not m:
        raise SyntaxError("Line must start with line number")
    lineno = int(m.group(1)); rest = m.group(2)
    s = rest
    i = 0
    tokens.append(Token('LINENUM', lineno, 0))

    while i < len(s):
        ch = peek()
        if ch.isspace():
            i += 1; continue
        if ch == '"':
            # string literal
            i += 1
            start = i
            buf = []
            while i < len(s):
                if s[i] == '"':
                    i += 1
                    break
                buf.append(s[i]); i += 1
            tokens.append(Token('STR', ''.join(buf), i))
            continue
        if ch.isdigit():
            m = re.match(r'\d+(\.\d+)?', s[i:])
            val = m.group(0)
            i += len(val)
            tokens.append(Token('NUM', float(val) if '.' in val else int(val), i))
            continue
        if ch.isalpha() or ch == '_':
            m = re.match(r'[A-Za-z_][A-Za-z0-9_]*', s[i:])
            ident = m.group(0).upper()
            i += len(ident)
            # possible combined keywords (lookahead)
            if ident in {'END','DEF'}:
                j = i
                while j < len(s) and s[j].isspace(): j += 1
                m2 = re.match(r'(IF|SELECT|WHILE|FN)\b', s[j:].upper())
                if m2:
                    combined = COMBINED.get((ident, m2.group(1)), None)
                    if combined:
                        i = j + len(m2.group(1))
                        tokens.append(Token('KW', combined, i))
                        continue
            if ident in KEYWORDS:
                tokens.append(Token('KW', ident, i))
            else:
                tokens.append(Token('ID', ident, i))
            continue
        # operators & separators
        two = s[i:i+2]
        if two in ('<=','>=','<>'):
            tokens.append(Token('OP', two, i)); i += 2; continue
        if ch in '+-*/^=(),:;&':
            tokens.append(Token('OP', ch, i)); i += 1; continue
        # unknown char
        raise SyntaxError(f"Unknown char {ch} at {i}")
    tokens.append(Token('EOL','',len(s)))
    return tokens

# =========================
# Parser (recursive descent / Pratt for expressions)
# =========================
class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.i = 0

    def peek(self): return self.tokens[self.i]
    def match(self, typ=None, vals=None):
        t = self.peek()
        if typ and t.type != typ: return None
        if vals and t.val not in vals: return None
        self.i += 1
        return t

    def expect(self, typ=None, vals=None):
        t = self.peek()
        if (typ and t.type != typ) or (vals and t.val not in vals):
            raise SyntaxError(f"Expected {typ} {vals} but got {t}")
        self.i += 1
        return t

    def parse(self):
        # program = many lines
        lines = {}
        while self.i < len(self.tokens):
            if self.peek().type == 'EOL': self.i += 1; continue
            lineno = self.expect('LINENUM').val
            stmts = self.parse_statements_until_eol()
            lines[lineno] = stmts
        return lines

    def parse_statements_until_eol(self):
        stmts = [self.parse_statement()]
        while self.peek().type != 'EOL':
            if self.match('OP', {':'}):
                stmts.append(self.parse_statement())
            else:
                break
        self.expect('EOL')
        return stmts

    # ----- statements -----
    def parse_statement(self):
        t = self.peek()
        if t.type == 'KW':
            kw = t.val
            if kw == 'LET': self.i+=1; return self.parse_assignment()
            if kw == 'PRINT': self.i+=1; return ('PRINT', self.parse_print_list())
            if kw == 'INPUT': self.i+=1; return ('INPUT', self.parse_input_list())
            if kw == 'IF': self.i+=1; return self.parse_if()
            if kw == 'FOR': self.i+=1; return self.parse_for()
            if kw == 'NEXT': self.i+=1; var = self.match('ID'); return ('NEXT', var.val if var else None)
            if kw == 'GOTO': self.i+=1; line = self.expect('NUM').val; return ('GOTO', int(line))
            if kw == 'GOSUB': self.i+=1; line = self.expect('NUM').val; return ('GOSUB', int(line))
            if kw == 'RETURN': self.i+=1; return ('RETURN',)
            if kw == 'END': self.i+=1; return ('ENDP',)
            if kw == 'STOP': self.i+=1; return ('STOP',)
            if kw == 'DIM': self.i+=1; return ('DIM', self.parse_dim_list())
            if kw == 'DATA': self.i+=1; return ('DATA', self.parse_data_list())
            if kw == 'READ': self.i+=1; return ('READ', self.parse_var_list())
            if kw == 'RESTORE': self.i+=1; return ('RESTORE',)
            if kw == 'ON': self.i+=1; return self.parse_on_goto_gosub()
            if kw == 'SELECT': self.i+=1; self.expect('KW', {'CASE'}); return self.parse_select_case()
            if kw == 'WHILE': self.i+=1; cond = self.parse_expr(); return ('WHILE', cond)
            if kw == 'END_WHILE': self.i+=1; return ('END_WHILE',)
            if kw == 'REPEAT': self.i+=1; return ('REPEAT',)
            if kw == 'LOOP': self.i+=1; 
                # optional UNTIL at same line
                if self.match('KW', {'UNTIL'}):
                    cond = self.parse_expr()
                    return ('LOOP_UNTIL', cond)
                return ('LOOP',)
            if kw == 'DEF_FN': 
                self.i+=1; return self.parse_def_fn()
            if kw == 'FNEND':
                self.i+=1; return ('FNEND',)
            if kw == 'SUB':
                self.i+=1; return self.parse_sub()
            if kw == 'SUBEND':
                self.i+=1; return ('SUBEND',)
        # default: assignment or bare expression (allow function call without LET)
        return self.parse_assignment_or_expr_stmt()

    def parse_assignment_or_expr_stmt(self):
        # lookahead: ID '=' or function call like NAME(...)
        t = self.peek()
        if t.type == 'ID':
            # allow array element on LHS
            save = self.i
            name = self.match('ID').val
            # array index?
            if self.match('OP', {'('}):
                idx = self.parse_expr_list_until(')')
                if self.match('OP', {'='}):
                    expr = self.parse_expr()
                    return ('ASSIGN_IDX', name, idx, expr)
                else:
                    # function-call-like expr stmt
                    self.i = save
            else:
                if self.match('OP', {'='}):
                    expr = self.parse_expr()
                    return ('ASSIGN', name, expr)
                else:
                    self.i = save
        # fall back to expr-stmt (for user FN/SUB call without assignment)
        expr = self.parse_expr()
        return ('EXPR', expr)

    def parse_assignment(self):
        # after LET
        name = self.expect('ID').val
        if self.match('OP', {'('}):
            idx = self.parse_expr_list_until(')')
            self.expect('OP', {'='})
            expr = self.parse_expr()
            return ('ASSIGN_IDX', name, idx, expr)
        else:
            self.expect('OP', {'='})
            expr = self.parse_expr()
            return ('ASSIGN', name, expr)

    def parse_print_list(self):
        items = [self.parse_expr_or_strsep()]
        while self.match('OP', {','}):
            items.append(self.parse_expr_or_strsep())
        return items

    def parse_input_list(self):
        vars_ = [self.expect('ID').val]
        while self.match('OP', {','}):
            vars_.append(self.expect('ID').val)
        return vars_

    def parse_dim_list(self):
        dims = []
        while True:
            name = self.expect('ID').val
            self.expect('OP', {'('})
            idxs = self.parse_expr_list_until(')')
            dims.append((name, idxs))
            if not self.match('OP', {','}): break
        return dims

    def parse_data_list(self):
        items = [self.parse_data_item()]
        while self.match('OP', {','}):
            items.append(self.parse_data_item())
        return items

    def parse_data_item(self):
        t = self.peek()
        if t.type in ('NUM','STR'): self.i+=1; return t.val
        # allow bare identifiers as strings in DATA
        if t.type == 'ID': self.i+=1; return t.val
        raise SyntaxError("DATA expects number/string/identifier")

    def parse_var_list(self):
        vars_ = [self.expect('ID').val]
        while self.match('OP', {','}):
            vars_.append(self.expect('ID').val)
        return vars_

    def parse_on_goto_gosub(self):
        expr = self.parse_expr()
        kind = self.expect('KW', {'GOTO','GOSUB'}).val
        targets = [int(self.expect('NUM').val)]
        while self.match('OP', {','}):
            targets.append(int(self.expect('NUM').val))
        return ('ON', kind, expr, targets)

    def parse_select_case(self):
        expr = self.parse_expr()
        # body is captured as special marker; interpreter will scan until END_SELECT
        return ('SELECT_CASE_BEGIN', expr)

    def parse_def_fn(self):
        name = self.expect('ID').val
        self.expect('OP', {'('})
        params = []
        if not self.match('OP', {')'}):
            params.append(self.expect('ID').val)
            while self.match('OP', {','}):
                params.append(self.expect('ID').val)
            self.expect('OP', {')'})
        return ('DEF_FN', name, params)

    def parse_sub(self):
        name = self.expect('ID').val
        self.expect('OP', {'('})
        params = []
        if not self.match('OP', {')'}):
            params.append(self.expect('ID').val)
            while self.match('OP', {','}):
                params.append(self.expect('ID').val)
            self.expect('OP', {')'})
        return ('SUB', name, params)

    # ----- expressions -----
    # Pratt parser
    def parse_expr(self):
        return self.parse_or()

    def parse_or(self):
        node = self.parse_and()
        while self.match('KW', {'OR'}):
            rhs = self.parse_and()
            node = ('OR', node, rhs)
        return node

    def parse_and(self):
        node = self.parse_not()
        while self.match('KW', {'AND'}):
            rhs = self.parse_not()
            node = ('AND', node, rhs)
        return node

    def parse_not(self):
        if self.match('KW', {'NOT'}):
            return ('NOT', self.parse_not())
        return self.parse_cmp()

    def parse_cmp(self):
        node = self.parse_add()
        while True:
            t = self.peek()
            if t.type == 'OP' and t.val in ('=','<>','<','<=','>','>='):
                self.i += 1
                rhs = self.parse_add()
                node = ('CMP', t.val, node, rhs)
            else:
                break
        return node

    def parse_add(self):
        node = self.parse_mul()
        while True:
            t = self.peek()
            if t.type == 'OP' and t.val in ('+','-','&'):
                self.i += 1
                rhs = self.parse_mul()
                node = ('BIN', t.val, node, rhs)
            else:
                break
        return node

    def parse_mul(self):
        node = self.parse_pow()
        while True:
            t = self.peek()
            if t.type == 'OP' and t.val in ('*','/'):
                self.i += 1
                rhs = self.parse_pow()
                node = ('BIN', t.val, node, rhs)
            else:
                break
        return node

    def parse_pow(self):
        node = self.parse_unary()
        while self.match('OP', {'^'}):
            rhs = self.parse_unary()
            node = ('POW', node, rhs)
        return node

    def parse_unary(self):
        t = self.peek()
        if t.type == 'OP' and t.val in ('+','-'):
            self.i += 1
            return ('UN', t.val, self.parse_unary())
        return self.parse_primary()

    def parse_primary(self):
        t = self.peek()
        if t.type == 'NUM':
            self.i+=1; return ('NUM', t.val)
        if t.type == 'STR':
            self.i+=1; return ('STR', t.val)
        if t.type == 'ID':
            name = t.val; self.i+=1
            # function call / built-in / user FN/SUB
            if self.match('OP', {'('}):
                args = self.parse_expr_list_until(')')
                return ('CALL', name, args)
            # array ref?
            if self.match('OP', {'('}):
                idx = self.parse_expr_list_until(')')
                return ('ARR', name, idx)
            return ('VAR', name)
        if t.type == 'OP' and t.val == '(':
            self.i+=1
            node = self.parse_expr()
            self.expect('OP', {')'})
            return node
        raise SyntaxError(f"Unexpected token in expression: {t}")

    def parse_expr_or_strsep(self):
        # PRINT 전용: 연속적인 문자열 구분자 ';'를 허용
        t = self.peek()
        if t.type == 'STR':
            self.i+=1; return ('STR', t.val)
        if t.type == 'OP' and t.val == ';':
            self.i+=1; return ('SEMI',)
        return self.parse_expr()

    def parse_expr_list_until(self, end_op):
        args = []
        if not self.match('OP', {end_op}):
            args.append(self.parse_expr())
            while self.match('OP', {','}):
                args.append(self.parse_expr())
            self.expect('OP', {end_op})
        return args


# =========================
# Interpreter
# =========================
class BasicRuntime:
    def __init__(self, program_lines):
        # program_lines: dict lineno -> [statements]
        self.lines = dict(sorted(program_lines.items()))
        self.linenos = list(self.lines.keys())
        self.pc_index = 0  # index into linenos
        self.vars = {}
        self.arrays = {}
        self.data = []
        self.data_ptr = 0
        self.call_stack = []  # GOSUB return addresses (pc_index)
        self.for_stack = []   # tuples (varname, end, step, loop_line_index)
        self.while_stack = [] # (start_pc_index, condition_expr)
        self.repeat_stack = [] # start_pc_index
        self.subroutines = {} # name -> (params, body_lines)
        self.functions = {}   # name -> (params, body_lines)
        self.labels_cache = {} # lineno -> index

        # preprocess: extract DATA pool and SUB/DEF FN bodies
        self._extract_data_and_blocks()

    # Utilities
    def _lineno_to_index(self, target):
        if target in self.labels_cache:
            return self.labels_cache[target]
        # binary search
        try:
            idx = self.linenos.index(target)
        except ValueError:
            raise RuntimeError(f"Line {target} not found")
        self.labels_cache[target] = idx
        return idx

    def _extract_data_and_blocks(self):
        # Flatten DATA into global pool; also move SUB/DEF FN bodies into separate stores
        new_lines = {}
        capturing = None
        capture_name = None
        capture_params = None
        block_lines = []
        for ln in self.linenos:
            out = []
            for st in self.lines[ln]:
                typ = st[0]
                if capturing:
                    if typ == capturing+'_END':
                        # store block
                        if capturing == 'SUB':
                            self.subroutines[capture_name] = (capture_params, block_lines)
                        else:
                            self.functions[capture_name] = (capture_params, block_lines)
                        capturing = None; capture_name=None; capture_params=None; block_lines=[]
                    else:
                        block_lines.append((ln, st))
                    continue
                if typ == 'DATA':
                    self.data.extend(st[1])
                elif typ == 'SUB':
                    capturing = 'SUB'; capture_name = st[1]; capture_params = st[2]; block_lines=[]
                elif typ == 'SUBEND':
                    out.append(('SUB_END_MARK',))
                elif typ == 'DEF_FN':
                    capturing = 'FN'; capture_name = st[1]; capture_params = st[2]; block_lines=[]
                elif typ == 'FNEND':
                    out.append(('FN_END_MARK',))
                else:
                    out.append(st)
            if out:
                new_lines[ln] = out
        self.lines = new_lines
        self.linenos = list(self.lines.keys())

    # Expression evaluation
    def eval_expr(self, node):
        typ = node[0]
        if typ == 'NUM': return node[1]
        if typ == 'STR': return node[1]
        if typ == 'VAR':
            return self.vars.get(node[1], 0)
        if typ == 'ARR':
            name, idx_nodes = node[1], node[2]
            idx = tuple(int(self.eval_expr(n)) for n in idx_nodes)
            return self.arrays[name][idx]
        if typ == 'CALL':
            name, args_nodes = node[1], node[2]
            args = [self.eval_expr(a) for a in args_nodes]
            U = name.upper()
            # built-in numeric functions (arity flexible for RND)
            if U in BUILTINS:
                f = BUILTINS[U]
                return f(*args) if args else f()
            # user function?
            if U in self.functions:
                params, body = self.functions[U]
                if len(params) != len(args): raise RuntimeError("FN arg count mismatch")
                # create new scope (shallow)
                saved = dict(self.vars)
                for p,v in zip(params, args):
                    self.vars[p.upper()] = v
                # execute body; last evaluated expression in RETURN sets FN value
                ret = self._execute_block(body, fn_mode=True)
                self.vars = saved
                return ret
            # user subroutine call with args (allow as expr returns 0)
            if U in self.subroutines:
                self.call_subroutine(U, args)
                return 0
            # allow bare variable call (no-op) to be 0
            return 0
        if typ == 'UN':
            op, v = node[1], self.eval_expr(node[2])
            return +v if op=='+' else -v
        if typ == 'BIN':
            op, a, b = node[1], self.eval_expr(node[2]), self.eval_expr(node[3])
            if op == '&': return str(a) + str(b)
            if op == '+':
                if isinstance(a,str) or isinstance(b,str): return str(a)+str(b)
                return a+b
            if op == '-': return a-b
            if op == '*': return a*b
            if op == '/': return a/b
        if typ == 'POW':
            return self.eval_expr(node[1]) ** self.eval_expr(node[2])
        if typ == 'CMP':
            op, a, b = node[1], self.eval_expr(node[2]), self.eval_expr(node[3])
            if op == '=': return -1 if a==b else 0
            if op == '<>': return -1 if a!=b else 0
            if op == '<': return -1 if a<b else 0
            if op == '<=': return -1 if a<=b else 0
            if op == '>': return -1 if a>b else 0
            if op == '>=': return -1 if a>=b else 0
        if typ == 'AND':
            return -1 if (self.truthy(node[1]) and self.truthy(node[2])) else 0
        if typ == 'OR':
            return -1 if (self.truthy(node[1]) or self.truthy(node[2])) else 0
        if typ == 'NOT':
            return -1 if not self.truthy(node[1]) else 0
        raise RuntimeError(f"Unknown expr node {node}")

    def truthy(self, node):
        v = self.eval_expr(node) if isinstance(node, tuple) else node
        # HP BASIC는 true=-1, false=0 관례를 따름. 여기서는 0이 아니면 참으로 취급.
        return v != 0

    # Statement execution
    def run(self):
        self.pc_index = 0
        while self.pc_index < len(self.linenos):
            ln = self.linenos[self.pc_index]
            for st in self.lines[ln]:
                cont = self.exec_stmt(ln, st)
                if cont == 'JUMP': break
                if cont == 'HALT': return
            else:
                self.pc_index += 1

    def exec_stmt(self, ln, st):
        typ = st[0]
        if typ == 'ASSIGN':
            self.vars[st[1]] = self.eval_expr(st[2])
        elif typ == 'ASSIGN_IDX':
            name, idx_nodes, expr = st[1], st[2], st[3]
            idx = tuple(int(self.eval_expr(n)) for n in idx_nodes)
            self.arrays[name][idx] = self.eval_expr(expr)
        elif typ == 'EXPR':
            self.eval_expr(st[1])
        elif typ == 'PRINT':
            out_parts = []
            for item in st[1]:
                if item[0] == 'SEMI':
                    out_parts.append('')
                else:
                    val = self.eval_expr(item)
                    out_parts.append(str(val))
            print(' '.join(p for p in out_parts if p!=''))
        elif typ == 'INPUT':
            for v in st[1]:
                try:
                    s = input(f"? {v} = ")
                    if s.startswith('"') and s.endswith('"'):
                        self.vars[v] = s.strip('"')
                    else:
                        self.vars[v] = float(s) if ('.' in s) else int(s)
                except EOFError:
                    self.vars[v] = 0
        elif typ == 'IF':
            cond, then_part, else_part = st[1], st[2], st[3]
            if self.truthy(cond):
                return self.exec_inline_or_goto(then_part)
            elif else_part is not None:
                return self.exec_inline_or_goto(else_part)
        elif typ == 'FOR':
            var, start, end, step = st[1], self.eval_expr(st[2]), self.eval_expr(st[3]), self.eval_expr(st[4])
            self.vars[var] = start
            # loop returns to this line after body to increment/evaluate
            self.for_stack.append((var, end, step, self.pc_index))
        elif typ == 'NEXT':
            var = st[1]
            if not self.for_stack: raise RuntimeError("NEXT without FOR")
            vname, end, step, loop_idx = self.for_stack[-1]
            if var and vname != var: raise RuntimeError("Mismatched NEXT variable")
            self.vars[vname] = self.vars.get(vname, 0) + step
            if (step >= 0 and self.vars[vname] <= end) or (step < 0 and self.vars[vname] >= end):
                # jump back to after FOR line
                self.pc_index = loop_idx
                return 'JUMP'
            else:
                self.for_stack.pop()
        elif typ == 'GOTO':
            self.pc_index = self._lineno_to_index(st[1])
            return 'JUMP'
        elif typ == 'GOSUB':
            self.call_stack.append(self.pc_index)
            self.pc_index = self._lineno_to_index(st[1])
            return 'JUMP'
        elif typ == 'RETURN':
            if not self.call_stack: raise RuntimeError("RETURN without GOSUB")
            self.pc_index = self.call_stack.pop() + 1
            return 'JUMP'
        elif typ == 'ENDP' or typ == 'STOP':
            return 'HALT'
        elif typ == 'DIM':
            for name, idx_nodes in st[1]:
                shape = tuple(int(self.eval_expr(n)) for n in idx_nodes)
                # arrays are 1-based by tradition; use simple tuple keys
                self.arrays[name] = defaultdict(int)
        elif typ == 'DATA':
            pass  # already harvested
        elif typ == 'READ':
            for v in st[1]:
                if self.data_ptr >= len(self.data): raise RuntimeError("Out of DATA")
                self.vars[v] = self.data[self.data_ptr]; self.data_ptr += 1
        elif typ == 'RESTORE':
            self.data_ptr = 0
        elif typ == 'ON':
            kind, expr, targets = st[1], st[2], st[3]
            idx = int(self.eval_expr(expr))
            if idx < 1 or idx > len(targets): return
            target = targets[idx-1]
            if kind == 'GOTO':
                self.pc_index = self._lineno_to_index(target)
                return 'JUMP'
            else:
                self.call_stack.append(self.pc_index)
                self.pc_index = self._lineno_to_index(target)
                return 'JUMP'
        elif typ == 'SELECT_CASE_BEGIN':
            # scan forward to matching END_SELECT; choose matching CASE
            expr_val = self.eval_expr(st[1])
            chosen_index = None
            else_index = None
            scan_idx = self.pc_index
            while True:
                scan_idx += 1
                if scan_idx >= len(self.linenos): raise RuntimeError("Missing END SELECT")
                ln2 = self.linenos[scan_idx]
                for st2 in self.lines[ln2]:
                    if st2[0] == 'END_SELECT':
                        # no match found; jump to after end select
                        self.pc_index = scan_idx
                        return 'JUMP'
                    if st2[0] == 'CASE':
                        match = self._case_match(expr_val, st2[1])
                        if match and chosen_index is None:
                            chosen_index = scan_idx
                            break
                    if st2[0] == 'CASE_ELSE':
                        else_index = scan_idx
                if chosen_index is not None:
                    break
            # jump to first statement after matching CASE header
            self.pc_index = chosen_index
            return 'JUMP'
        elif typ == 'CASE':
            # just a header; do nothing
            pass
        elif typ == 'CASE_ELSE':
            pass
        elif typ == 'END_SELECT':
            pass
        elif typ == 'WHILE':
            cond = st[1]
            if self.truthy(cond):
                # push loop start (next line)
                self.while_stack.append((self.pc_index, cond))
            else:
                # skip to END_WHILE
                self._skip_until('END_WHILE')
        elif typ == 'END_WHILE':
            if not self.while_stack: return
            start_idx, cond = self.while_stack[-1]
            if self.truthy(cond):
                self.pc_index = start_idx
                return 'JUMP'
            else:
                self.while_stack.pop()
        elif typ == 'REPEAT':
            self.repeat_stack.append(self.pc_index)
        elif typ == 'LOOP':
            if not self.repeat_stack: raise RuntimeError("LOOP without REPEAT")
            start_idx = self.repeat_stack[-1]
            self.pc_index = start_idx
            return 'JUMP'
        elif typ == 'LOOP_UNTIL':
            if not self.repeat_stack: raise RuntimeError("LOOP UNTIL without REPEAT")
            cond = st[1]
            if not self.truthy(cond):
                start_idx = self.repeat_stack[-1]
                self.pc_index = start_idx
                return 'JUMP'
            else:
                self.repeat_stack.pop()
        else:
            raise RuntimeError(f"Unknown stmt {st}")
        return None

    def exec_inline_or_goto(self, part):
        # part is either ('LINE', lineno) or a single statement AST
        if isinstance(part, tuple) and part[0]=='LINE':
            self.pc_index = self._lineno_to_index(part[1])
            return 'JUMP'
        else:
            # execute a single "inline" statement
            self.exec_stmt(self.linenos[self.pc_index], part)
            return None

    def _skip_until(self, end_kw):
        scan_idx = self.pc_index
        while True:
            scan_idx += 1
            if scan_idx >= len(self.linenos): raise RuntimeError(f"Missing {end_kw}")
            ln2 = self.linenos[scan_idx]
            for st2 in self.lines[ln2]:
                if st2[0] == end_kw:
                    self.pc_index = scan_idx
                    return

    def _case_match(self, value, selector):
        # selector can be list of values / ranges
        # support: CASE 1,2,5 TO 10, IS >= 3  (we implement simple forms)
        for item in selector:
            if isinstance(item, tuple) and item[0]=='RANGE':
                lo, hi = item[1], item[2]
                if lo <= value <= hi: return True
            else:
                if value == item: return True
        return False

    def _execute_block(self, body_lines, fn_mode=False):
        # Execute a captured SUB/FN block until SUBEND/FNEND encountered
        saved_pc, saved_lines, saved_linenos = self.pc_index, self.lines, self.linenos
        # create a tiny ephemeral program of the block
        ep_lines = defaultdict(list)
        fake_line_order = []
        for i,(ln, st) in enumerate(body_lines):
            fake_ln = 100000 + i
            ep_lines[fake_ln] = [st]
            fake_line_order.append(fake_ln)
        self.lines = ep_lines
        self.linenos = fake_line_order
        self.pc_index = 0
        ret_val = 0
        while self.pc_index < len(self.linenos):
            ln = self.linenos[self.pc_index]
            st = self.lines[ln][0]
            if fn_mode and st[0]=='RETURN':
                # treat RETURN expr; allow RETURN x inside FN
                ret_val = self.vars.get('_FN_RET', 0)
            if (fn_mode and st[0]=='FNEND') or (not fn_mode and st[0]=='SUBEND'):
                break
            self.exec_stmt(ln, st)
            self.pc_index += 1
        # restore
        self.lines, self.linenos, self.pc_index = saved_lines, saved_linenos, saved_pc
        return ret_val

    def call_subroutine(self, name, args):
        params, body = self.subroutines[name]
        if len(params) != len(args): raise RuntimeError("SUB arg count mismatch")
        saved = dict(self.vars)
        for p,v in zip(params, args):
            self.vars[p.upper()] = v
        self._execute_block(body, fn_mode=False)
        self.vars = saved

# -------------- high-level helpers --------------
def parse_program(text):
    # tokenize each physical line; then feed to Parser
    tokens = []
    for raw in text.splitlines():
        raw = raw.rstrip()
        if raw.strip()=='' or raw.strip().startswith("'"):  # allow apostrophe comments
            continue
        toks = tokenize_line(raw)
        tokens.extend(toks)
    parser = Parser(tokens)
    return parser.parse()

def run_basic(text):
    program = parse_program(text)
    rt = BasicRuntime(program)
    rt.run()


# =========================
# Minimal SELECT CASE helpers in parser (post-parse patch)
# =========================
# We need to post-process SELECT/CASE blocks to produce explicit CASE, CASE_ELSE, END_SELECT markers.
# For simplicity, we capture syntax in the original parse via line statements. But we didn't yet
# build CASE ... lists. We'll add a small pass to convert textual CASE to AST form.

def enrich_select_case(program):
    # Convert CASE lines like: "CASE 1, 2, 5 TO 10" or "CASE ELSE"
    # We'll do a crude parse of CASE headers inside the stored statements.
    def parse_case_header(tokens_str):
        # supports constants and "n TO m"
        out = []
        toks = [t.strip().upper() for t in re.split(r'[,\s]+', tokens_str) if t.strip()!='']
        i=0
        while i < len(toks):
            if i+2 < len(toks) and toks[i+1]=='TO':
                lo = float(toks[i]) if '.' in toks[i] else int(toks[i])
                hi = float(toks[i+2]) if '.' in toks[i+2] else int(toks[i+2])
                out.append(('RANGE', lo, hi))
                i += 3
            else:
                v = toks[i]
                if v.replace('.','',1).isdigit():
                    out.append(float(v) if '.' in v else int(v))
                else:
                    # fallback: treat as string literal
                    out.append(v)
                i += 1
        return out

    for ln, stmts in list(program.items()):
        new = []
        i=0
        while i < len(stmts):
            st = stmts[i]
            if st[0]=='SELECT_CASE_BEGIN':
                # rewrite following lines until END SELECT
                new.append(st)
                i += 1
                while i < len(stmts):
                    st2 = stmts[i]
                    if st2[0]=='KW' and st2[1]=='CASE':
                        i += 1
                    if st2[0]=='END_SELECT':
                        new.append(st2); i+=1; break
                    i+=1
            new.append(st)
            i+=1
        program[ln] = stmts
    return program