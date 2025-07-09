import fitz  # PyMuPDF
import re
from collections import defaultdict

def extract_figures_tables(pdf_path, search_height=200):
    doc = fitz.open(pdf_path)
    extracted = {}

    for page_num in range(len(doc)):
        page = doc[page_num]
        blocks = page.get_text("blocks")  # 텍스트 블록: (x0, y0, x1, y1, "text", block_no)

        for block in blocks:
            rect = fitz.Rect(block[:4])
            text = block[4].strip()
            # Figure or Table 캡션 찾기
            match = re.search(r"(Figure|Table)\s*\d+", text, re.IGNORECASE)
            if match:
                label = match.group().replace(" ", "_")  # e.g. "Figure_1"

                # 위쪽 영역을 추출: 캡션 블록 바로 위가 실제 이미지일 가능성이 높음
                capture_rect = fitz.Rect(
                    rect.x0,
                    max(0, rect.y0 - search_height),
                    rect.x1,
                    rect.y0
                )

                # 투명 배경으로 렌더링
                pix = page.get_pixmap(clip=capture_rect, dpi=200, alpha=True)
                image_bytes = pix.tobytes("png")

                # 중복 방지: 같은 label이 여러 번 나올 수 있으므로 페이지 번호도 포함
                key = f"{label}_p{page_num+1}"
                extracted[key] = {
                    "page": page_num + 1,
                    "image": image_bytes,
                    "ext": "png"
                }

    return extracted