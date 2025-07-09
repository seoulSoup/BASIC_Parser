import fitz  # PyMuPDF
import re

def extract_figures_from_pdf(pdf_path):
    doc = fitz.open(pdf_path)
    figures = {}

    for page_num in range(len(doc)):
        page = doc[page_num]
        image_list = page.get_images(full=True)

        # 1. 텍스트 추출
        text = page.get_text()
        captions = re.findall(r'(Figure\s*\d+|Fig\.\s*\d+|그림\s*\d+)', text, re.IGNORECASE)

        # 2. 이미지 추출 및 저장
        for img_idx, img in enumerate(image_list):
            xref = img[0]
            base_image = doc.extract_image(xref)
            img_bytes = base_image['image']
            ext = base_image['ext']

            # 3. 키 결정
            if img_idx < len(captions):
                key = captions[img_idx]
            else:
                key = f'page_{page_num+1}_img{img_idx+1}'

            # 4. 딕셔너리에 저장
            figures[key] = {
                'page': page_num + 1,
                'image': img_bytes,
                'ext': ext
            }

    return figures