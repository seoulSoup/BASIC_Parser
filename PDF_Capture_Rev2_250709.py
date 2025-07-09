from lark import Lark, Transformer, v_args
import re

# 방법 1: 문법에서 직접 다중 단어 키워드 정의
basic_grammar_v1 = """
    start: statement+
    
    statement: if_statement
             | assignment
             | print_statement
             | end_statement
             | empty_line
    
    if_statement: "IF" expression "THEN" NEWLINE
                    statement_block?
                  ("ELSE" "IF" expression "THEN" NEWLINE statement_block?)*
                  ("ELSE" NEWLINE statement_block?)?
                  "END" "IF" NEWLINE
    
    statement_block: statement+
    
    assignment: IDENTIFIER "=" expression NEWLINE
    print_statement: "PRINT" expression NEWLINE
    end_statement: "END" NEWLINE
    empty_line: NEWLINE
    
    expression: term (add_op term)*
    term: factor (mul_op factor)*
    factor: NUMBER | IDENTIFIER | "(" expression ")" | string
    
    add_op: "+" | "-"
    mul_op: "*" | "/"
    
    string: ESCAPED_STRING
    
    IDENTIFIER: /[a-zA-Z][a-zA-Z0-9]*/
    NUMBER: /\d+(\.\d+)?/
    ESCAPED_STRING: /"[^"]*"/
    NEWLINE: /\r?\n/
    
    %import common.WS
    %ignore WS
"""

# 방법 2: 전처리를 통한 다중 단어 키워드 처리
def preprocess_basic_code(code):
    """BASIC 코드를 전처리하여 다중 단어 키워드를 단일 토큰으로 변환"""
    # 대소문자를 구분하지 않는 패턴 매칭
    replacements = {
        r'\bEND\s+IF\b': 'ENDIF',
        r'\bELSE\s+IF\b': 'ELSEIF',
        r'\bFOR\s+EACH\b': 'FOREACH',
        r'\bNEXT\s+TO\b': 'NEXTTO',
        r'\bGO\s+TO\b': 'GOTO',
        r'\bGO\s+SUB\b': 'GOSUB',
    }
    
    processed_code = code.upper()  # BASIC은 대소문자를 구분하지 않음
    
    for pattern, replacement in replacements.items():
        processed_code = re.sub(pattern, replacement, processed_code, flags=re.IGNORECASE)
    
    return processed_code

# 전처리된 코드용 문법
basic_grammar_v2 = """
    start: statement+
    
    statement: if_statement
             | assignment
             | print_statement
             | end_statement
             | empty_line
    
    if_statement: "IF" expression "THEN" NEWLINE
                    statement_block?
                  ("ELSEIF" expression "THEN" NEWLINE statement_block?)*
                  ("ELSE" NEWLINE statement_block?)?
                  "ENDIF" NEWLINE
    
    statement_block: statement+
    
    assignment: IDENTIFIER "=" expression NEWLINE
    print_statement: "PRINT" expression NEWLINE
    end_statement: "END" NEWLINE
    empty_line: NEWLINE
    
    expression: term (add_op term)*
    term: factor (mul_op factor)*
    factor: NUMBER | IDENTIFIER | "(" expression ")" | string
    
    add_op: "+" | "-"
    mul_op: "*" | "/"
    
    string: ESCAPED_STRING
    
    IDENTIFIER: /[a-zA-Z][a-zA-Z0-9]*/
    NUMBER: /\d+(\.\d+)?/
    ESCAPED_STRING: /"[^"]*"/
    NEWLINE: /\r?\n/
    
    %import common.WS
    %ignore WS
"""

# 방법 3: 커스텀 렉서를 사용한 방법
class BasicLexer:
    """BASIC 언어용 커스텀 렉서"""
    
    def __init__(self):
        self.multi_word_keywords = {
            'END IF': 'ENDIF',
            'ELSE IF': 'ELSEIF',
            'GO TO': 'GOTO',
            'GO SUB': 'GOSUB',
            'FOR EACH': 'FOREACH',
        }
    
    def tokenize(self, text):
        """텍스트를 토큰화하여 다중 단어 키워드 처리"""
        # 먼저 다중 단어 키워드를 찾아서 대체
        processed_text = text.upper()
        
        for keyword, replacement in self.multi_word_keywords.items():
            # 단어 경계를 고려한 정규식 패턴
            pattern = r'\b' + re.escape(keyword) + r'\b'
            processed_text = re.sub(pattern, replacement, processed_text, flags=re.IGNORECASE)
        
        return processed_text

# AST 변환기
class BasicTransformer(Transformer):
    def __init__(self):
        super().__init__()
    
    @v_args(inline=True)
    def assignment(self, identifier, expression):
        return {'type': 'assignment', 'var': identifier, 'value': expression}
    
    @v_args(inline=True)
    def print_statement(self, expression):
        return {'type': 'print', 'value': expression}
    
    def if_statement(self, args):
        # IF 문의 구조를 파싱
        condition = args[0]
        then_block = []
        else_blocks = []
        else_block = []
        
        # 복잡한 IF 구조 파싱 로직
        return {
            'type': 'if',
            'condition': condition,
            'then': then_block,
            'elseif': else_blocks,
            'else': else_block
        }
    
    def expression(self, args):
        if len(args) == 1:
            return args[0]
        # 표현식 처리 로직
        return {'type': 'expression', 'parts': args}
    
    def factor(self, args):
        return args[0]
    
    def NUMBER(self, token):
        return int(token)
    
    def IDENTIFIER(self, token):
        return str(token)

# 사용 예제
def test_basic_parser():
    # 테스트용 BASIC 코드 (줄바꿈 문제 해결)
    basic_code = """X = 10
IF X > 5 THEN
    PRINT X
ELSE IF X > 0 THEN
    PRINT "POSITIVE"
ELSE
    PRINT "ZERO OR NEGATIVE"
END IF
END
"""
    
    print("원본 코드:")
    print(repr(basic_code))
    print(basic_code)
    
    # 방법 1: 문법에서 직접 처리
    print("\n=== 방법 1: 문법에서 직접 처리 ===")
    try:
        parser1 = Lark(basic_grammar_v1, parser='lalr')
        tree1 = parser1.parse(basic_code)
        print("파싱 성공!")
        print(tree1.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")
    
    # 방법 2: 전처리 사용
    print("\n=== 방법 2: 전처리 사용 ===")
    try:
        preprocessed_code = preprocess_basic_code(basic_code)
        print("전처리된 코드:")
        print(repr(preprocessed_code))
        print(preprocessed_code)
        
        parser2 = Lark(basic_grammar_v2, parser='lalr')
        tree2 = parser2.parse(preprocessed_code)
        print("파싱 성공!")
        print(tree2.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")
    
    # 방법 3: 커스텀 렉서 사용
    print("\n=== 방법 3: 커스텀 렉서 사용 ===")
    try:
        lexer = BasicLexer()
        tokenized_code = lexer.tokenize(basic_code)
        print("토큰화된 코드:")
        print(repr(tokenized_code))
        print(tokenized_code)
        
        parser3 = Lark(basic_grammar_v2, parser='lalr')
        tree3 = parser3.parse(tokenized_code)
        print("파싱 성공!")
        print(tree3.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")

# 더 간단한 테스트
def test_simple():
    simple_code = """X = 5 * 3
PRINT X
END
"""
    
    print("=== 간단한 테스트 ===")
    print("코드:")
    print(simple_code)
    
    try:
        parser = Lark(basic_grammar_v2, parser='lalr')
        tree = parser.parse(simple_code)
        print("파싱 성공!")
        print(tree.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")
        import traceback
        traceback.print_exc()

# IF문 전용 테스트
def test_if_statement():
    if_code = """IF X > 5 THEN
PRINT "BIG"
ENDIF
"""
    
    print("=== IF문 테스트 ===")
    print("코드:")
    print(repr(if_code))
    print(if_code)
    
    try:
        preprocessed = preprocess_basic_code(if_code)
        print("전처리된 코드:")
        print(repr(preprocessed))
        print(preprocessed)
        
        parser = Lark(basic_grammar_v2, parser='lalr')
        tree = parser.parse(preprocessed)
        print("파싱 성공!")
        print(tree.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")
        import traceback
        traceback.print_exc()

# 더 안전한 문법 (최소한의 구조)
safe_grammar = """
    start: statement+
    
    statement: assignment
             | print_statement
             | end_statement
             | empty_line
    
    assignment: IDENTIFIER "=" expression NEWLINE
    print_statement: "PRINT" expression NEWLINE
    end_statement: "END" NEWLINE
    empty_line: NEWLINE
    
    expression: term (add_op term)*
    term: factor (mul_op factor)*
    factor: NUMBER | IDENTIFIER | "(" expression ")" | string
    
    add_op: "+" | "-"
    mul_op: "*" | "/"
    
    string: ESCAPED_STRING
    
    IDENTIFIER: /[a-zA-Z][a-zA-Z0-9]*/
    NUMBER: /\d+(\.\d+)?/
    ESCAPED_STRING: /"[^"]*"/
    NEWLINE: /\r?\n/
    
    %import common.WS
    %ignore WS
"""

def test_safe_grammar():
    safe_code = """X = 5 * 3
PRINT X
END
"""
    
    print("=== 안전한 문법 테스트 ===")
    print("코드:")
    print(safe_code)
    
    try:
        parser = Lark(safe_grammar, parser='lalr')
        tree = parser.parse(safe_code)
        print("파싱 성공!")
        print(tree.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_safe_grammar()  # 가장 안전한 문법부터
    print("\n" + "="*50 + "\n")
    test_simple()  # 기본 테스트
    print("\n" + "="*50 + "\n")
    test_if_statement()  # IF문 테스트
    print("\n" + "="*50 + "\n")
    test_basic_parser()  # 전체 테스트