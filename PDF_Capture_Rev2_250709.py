from lark import Lark, Transformer, v_args
import re

# 방법 1: 문법에서 직접 다중 단어 키워드 정의
basic_grammar_v1 = """
    start: statement+
    
    statement: if_statement
             | assignment
             | print_statement
             | end_statement
             | NEWLINE
    
    if_statement: "IF" expression "THEN" NEWLINE
                    statement*
                  ("ELSE" "IF" expression "THEN" NEWLINE statement*)*
                  ("ELSE" NEWLINE statement*)?
                  "END" "IF" NEWLINE
    
    assignment: IDENTIFIER "=" expression NEWLINE
    print_statement: "PRINT" expression NEWLINE
    end_statement: "END" NEWLINE
    
    expression: term (("+" | "-") term)*
    term: factor (("*" | "/") factor)*
    factor: NUMBER | IDENTIFIER | "(" expression ")"
    
    IDENTIFIER: /[a-zA-Z][a-zA-Z0-9]*/
    NUMBER: /\d+/
    NEWLINE: /\n/
    
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
             | NEWLINE
    
    if_statement: "IF" expression "THEN" NEWLINE
                    statement*
                  ("ELSEIF" expression "THEN" NEWLINE statement*)*
                  ("ELSE" NEWLINE statement*)?
                  "ENDIF" NEWLINE
    
    assignment: IDENTIFIER "=" expression NEWLINE
    print_statement: "PRINT" expression NEWLINE
    end_statement: "END" NEWLINE
    
    expression: term (("+" | "-") term)*
    term: factor (("*" | "/") factor)*
    factor: NUMBER | IDENTIFIER | "(" expression ")"
    
    IDENTIFIER: /[a-zA-Z][a-zA-Z0-9]*/
    NUMBER: /\d+/
    NEWLINE: /\n/
    
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
    # 테스트용 BASIC 코드
    basic_code = """
    X = 10
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
        print(tokenized_code)
        
        parser3 = Lark(basic_grammar_v2, parser='lalr')
        tree3 = parser3.parse(tokenized_code)
        print("파싱 성공!")
        print(tree3.pretty())
    except Exception as e:
        print(f"파싱 실패: {e}")

if __name__ == "__main__":
    test_basic_parser()