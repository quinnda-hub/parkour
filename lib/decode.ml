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
