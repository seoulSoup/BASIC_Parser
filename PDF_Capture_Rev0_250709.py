import fitz  # PyMuPDF
import re

def extract_figure_tables(pdf_path):
    doc = fitz.open(pdf_path)
    output = {}

    for page_num in range(len(doc)):
        page = doc[page_num]

        # 1. 텍스트 블록 단위 추출
        blocks = page.get_text("blocks")  # (x0, y0, x1, y1, text, block_no)
        fig_blocks = [(b[:4], b[4]) for b in blocks if re.search(r"(Figure|Table)\s*\d+", b[4], re.IGNORECASE)]

        for idx, (bbox, text) in enumerate(fig_blocks):
            key = re.search(r"(Figure|Table)\s*\d+", text, re.IGNORECASE).group()

            # 2. 주변 박스 위로 이미지가 있을 것이라고 가정하고 사각형 위 영역 설정
            caption_rect = fitz.Rect(*bbox)
            search_rect = fitz.Rect(caption_rect.x0, caption_rect.y0 - 200, caption_rect.x1, caption_rect.y0)

            # 3. 해당 영역 잘라내기
            clip = page.get_pixmap(clip=search_rect, dpi=200)
            img_bytes = clip.tobytes("png")

            output[key] = {
                "page": page_num + 1,
                "image": img_bytes,
                "ext": "png"
            }

    return output