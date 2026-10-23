type decoded = int

let invalid = -1
let code d = if d land 0xff = 0xff then invalid else d lsr 8

let length d =
  let l = d land 0xff in
  if l = 0xff then 1 else l

let decode s i =
  let c = Char.code (String.unsafe_get s i) in
  if c < 0x80 then (c lsl 8) lor 1
  else
    let d = String.get_utf_8_uchar s i in
    if Uchar.utf_decode_is_valid d then
      (Uchar.to_int (Uchar.utf_decode_uchar d) lsl 8)
      lor Uchar.utf_decode_length d
    else 0xff

let zero_ranges =
  [|
    (0x0300, 0x036F);
    (0x1AB0, 0x1AFF);
    (0x1DC0, 0x1DFF);
    (0x20D0, 0x20FF);
    (0xFE20, 0xFE2F);
    (0x200B, 0x200F);
    (0xFE00, 0xFE0F);
    (0xE0100, 0xE01EF);
  |]

let wide_ranges =
  [|
    (0x1100, 0x115F);
    (0x2E80, 0x303E);
    (0x3041, 0x33FF);
    (0x3400, 0x4DBF);
    (0x4E00, 0x9FFF);
    (0xA000, 0xA4CF);
    (0xAC00, 0xD7A3);
    (0xF900, 0xFAFF);
    (0xFE30, 0xFE4F);
    (0xFF00, 0xFF60);
    (0xFFE0, 0xFFE6);
    (0x1F300, 0x1F64F);
    (0x1F900, 0x1F9FF);
    (0x20000, 0x2FFFD);
    (0x30000, 0x3FFFD);
  |]

let in_ranges r c = Array.exists (fun (lo, hi) -> lo <= c && c <= hi) r

let of_code c =
  if c < 0x300 then 1
  else if in_ranges zero_ranges c then 0
  else if in_ranges wide_ranges c then 2
  else 1
