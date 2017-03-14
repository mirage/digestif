module Bigstring = Digestif_bigstring

module Bytes =
struct
  include Bytes

  type v = { off : int
           ; len : int
           ; buf : t }

  external get_u16 : t -> int -> int   = "%caml_string_get16u"
  external get_u32 : t -> int -> int32 = "%caml_string_get32u"
  external get_u64 : t -> int -> int64 = "%caml_string_get64u"
  external set_u16 : t -> int -> int -> unit   = "%caml_string_set16u"
  external set_u32 : t -> int -> int32 -> unit = "%caml_string_set32u"
  external set_u64 : t -> int -> int64 -> unit = "%caml_string_set64u"

  let blit_bigstring src src_off dst dst_off len =
    for i = 0 to len - 1
    do set dst (dst_off + i) (Bigstring.get src (src_off + i)) done

  let rpad a size x =
    let l = length a
    and b = create size in
    blit a 0 b 0 l;
    fill b l (size - l) x;
    b
end

type t =
  | Bytes of Bytes.t
  | Bigstring of Bigstring.t

let bytes n = Bytes (Bytes.create n)
let bigstring n = Bigstring (Bigstring.create n)

let length = function
  | Bytes v -> Bytes.length v
  | Bigstring v -> Bigstring.length v

let get = function
  | Bytes v -> Bytes.get v
  | Bigstring v -> Bigstring.get v

let set = function
  | Bytes v -> Bytes.set v
  | Bigstring v -> Bigstring.set v

let get_u16 = function
  | Bytes v -> Bytes.get_u16 v
  | Bigstring v -> Bigstring.get_u16 v

let get_u32 = function
  | Bytes v -> Bytes.get_u32 v
  | Bigstring v -> Bigstring.get_u32 v

let get_u64 = function
  | Bytes v -> Bytes.get_u64 v
  | Bigstring v -> Bigstring.get_u64 v

let set_u16 = function
  | Bytes v -> Bytes.set_u16 v
  | Bigstring v -> Bigstring.set_u16 v

let set_u32 = function
  | Bytes v -> Bytes.set_u32 v
  | Bigstring v -> Bigstring.set_u32 v

let set_u64 = function
  | Bytes v -> Bytes.set_u64 v
  | Bigstring v -> Bigstring.set_u64 v

let sub = function
  | Bytes v -> fun off len -> Bytes (Bytes.sub v off len)
  | Bigstring v -> fun off len -> Bigstring (Bigstring.sub v off len)

let blit src src_off dst dst_off len =
  match src, dst with
  | Bytes src, Bytes dst -> Bytes.blit src src_off dst dst_off len
  | Bigstring src, Bigstring dst -> Bigstring.blit src src_off dst dst_off len
  | Bytes src, Bigstring dst -> Bigstring.blit_bytes src src_off dst dst_off len
  | Bigstring src, Bytes dst -> Bytes.blit_bigstring src src_off dst dst_off len
