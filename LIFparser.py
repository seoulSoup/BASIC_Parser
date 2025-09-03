def lif_list_names(path, entry_size=32, name_field_size=16, max_entries=64, encodings=None):
    """
    LIF 컨테이너처럼 고정길이 엔트리를 순회하며
    각 엔트리의 '이름 필드'를 여러 인코딩으로 디코딩해 보여줍니다.
    """
    results = []
    with open(path, 'rb') as f:
        data = f.read(entry_size * max_entries)

    for i in range(0, len(data), entry_size):
        entry = data[i:i+entry_size]
        if len(entry) < name_field_size:
            break
        name_raw = entry[:name_field_size]
        # 엔트리가 모두 0이면 끝으로 간주
        if set(name_raw) <= {0, 32}:  # 0x00, 공백
            continue
        decodes = try_decodes(name_raw, encodings=encodings)
        results.append({
            "offset": i,
            "raw_hex": " ".join(f"{b:02x}" for b in name_raw),
            **decodes
        })
    return results

# 예시 출력 포맷팅
def print_lif_names(results, show_codecs=("ascii","latin-1","roman8","hp-roman8","utf-8","cp949")):
    for r in results:
        print(f"[offset 0x{r['offset']:08x}] name_raw: {r['raw_hex']}")
        for enc in show_codecs:
            if enc in r:
                print(f"  - {enc:9s}: {r[enc]}")
        print()