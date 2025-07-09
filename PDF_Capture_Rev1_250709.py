import fitz
import re

def extract_figures_using_xref(pdf_path):
    doc = fitz.open(pdf_path)
    results = {}

    for page_num, page in enumerate(doc):
        images = page.get_images(full=True)
        blocks = page.get_text("blocks")

        # 캡션 텍스트 추출
        caption_blocks = [
            (fitz.Rect(b[:4]), b[4].strip())
            for b in blocks
            if re.search(r'(Figure|Table)\s*\d+', b[4], re.IGNORECASE)
        ]

        for img_index, img in enumerate(images):
            xref = img[0]
            img_info = doc.extract_image(xref)
            image_bytes = img_info["image"]
            ext = img_info["ext"]
            rects = page.get_image_rects(xref)

            if not rects:
                continue

            image_rect = rects[0]  # 첫 번째 위치만 사용 (일반적으로 하나)

            # 이미지와 가까운 캡션 찾기
            min_dist = float("inf")
            best_caption = None
            for caption_rect, caption_text in caption_blocks:
                vertical_dist = caption_rect.y0 - image_rect.y1
                if 0 <= vertical_dist <= 100:  # 이미지 바로 아래 캡션이 있을 때
                    if vertical_dist < min_dist:
                        min_dist = vertical_dist
                        best_caption = caption_text

            # 캡션이 매칭되면 저장
            if best_caption:
                key = f"{best_caption.replace(' ', '_')}_p{page_num+1}"
            else:
                key = f"page{page_num+1}_img{img_index+1}"

            results[key] = {
                "page": page_num + 1,
                "image": image_bytes,
                "ext": ext
            }

    return results