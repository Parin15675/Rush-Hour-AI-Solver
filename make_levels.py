# tools/make_levels.py
import json, os, re, urllib.request
from collections import defaultdict

# --- ตั้งค่าพื้นฐาน ---
OUT_DIR = "./puzzles"        # ปรับ path ถ้าโครงสร้างต่าง
NEEDED = 20                   # เอา 10 ด่าน
MIN_MOVES, MAX_MOVES = 30, 40
DB_PREVIEW_URL = "https://www.michaelfogleman.com/static/rush/rush1000.txt"
# ถ้าคุณโหลดไฟล์ใหญ่ rush.txt.gz ไว้ในเครื่อง ให้ชี้ path แทน DB_PREVIEW_URL ก็ได้

def fetch_lines():
    try:
        with urllib.request.urlopen(DB_PREVIEW_URL) as f:
            data = f.read().decode("utf-8", errors="ignore")
        return [ln.strip() for ln in data.splitlines() if ln.strip()]
    except Exception as e:
        raise SystemExit(f"ดาวน์โหลดฐานข้อมูลไม่ได้: {e}\n"
                         f"คุณอาจโหลดไฟล์เต็มเองจากหน้า Database Download แล้วชี้ path แทน URL นี้ก็ได้.")

def parse_board(board_str):
    """คืน dict: key=letter, value=list ของ (r,c) ที่อักขระนั้นครอบครอง"""
    cells = defaultdict(list)
    if len(board_str) != 36:
        raise ValueError("board string ต้องยาว 36 อักขระ")
    idx = 0
    for r in range(6):
        for c in range(6):
            ch = board_str[idx]
            idx += 1
            if ch in ".o":
                continue
            if ch == "x":  # เราไม่ใช้กำแพง
                raise ValueError("มี x (wall) ในบอร์ดนี้")
            cells[ch].append((r, c))
    return cells

def to_json_puzzle(cells):
    """แปลงจากตำแหน่งเซลล์ -> JSON แบบเกมคุณ (x,y จากมุมซ้ายบน, dir H/V, len 2/3)"""
    out = {}
    for ch, coords in cells.items():
        # หาค่าต่ำสุดแถว/คอลัมน์เป็นตำแหน่ง anchor
        rows = sorted(set(r for r, _ in coords))
        cols = sorted(set(c for _, c in coords))
        if len(rows) == 1:
            direction = "H"
            length = len(cols)
            x, y = min(cols), rows[0]
        elif len(cols) == 1:
            direction = "V"
            length = len(rows)
            x, y = cols[0], min(rows)
        else:
            raise ValueError(f"ชิ้น {ch} มีทรงไม่ถูกต้อง (ไม่เป็นเส้นตรง)")

        if length not in (2, 3):
            raise ValueError(f"ชิ้น {ch} ความยาว {length} ไม่ใช่ 2 หรือ 3")

        key = "R" if ch == "A" else ch.upper()
        out[key] = {"x": x, "y": y, "dir": direction, "len": length}
    # ยืนยันว่ามีรถแดง
    if "R" not in out:
        raise ValueError("ไม่มีรถแดง (A) ในบอร์ดนี้")
    return out

def main():
    os.makedirs(OUT_DIR, exist_ok=True)
    picked = 0
    for ln in fetch_lines():
        # รูปแบบ: "<moves> <board> <cluster>"
        m = re.match(r"^\s*(\d+)\s+([A-Za-z\.ox]{36})\s+(\d+)\s*$", ln)
        if not m:
            continue
        moves = int(m.group(1))
        board = m.group(2)
        if not (MIN_MOVES <= moves <= MAX_MOVES):
            continue
        if "x" in board:   # ตัดบอร์ดที่มีกำแพง
            continue
        try:
            cells = parse_board(board)
            puzzle = to_json_puzzle(cells)
        except Exception:
            continue  # ข้ามเคสผิดรูป

        picked += 1
        out_path = os.path.join(OUT_DIR, f"level_{picked:02d}.json")
        with open(out_path, "w", encoding="utf-8") as f:
            json.dump(puzzle, f, ensure_ascii=False, indent=2)
        print(f"[OK] moves={moves} -> {out_path}")
        if picked >= NEEDED:
            break

    if picked < NEEDED:
        print(f"\nได้มา {picked} ด่านจาก preview. "
              f"แนะนำโหลดไฟล์เต็มจากหน้า Database Download แล้วปรับตัวแปร DB_PREVIEW_URL "
              f"เป็น path ไฟล์ในเครื่อง เพื่อให้มีตัวเลือกมากขึ้น (จะมีด่าน 30–40 เพียบ).")

if __name__ == "__main__":
    main()
