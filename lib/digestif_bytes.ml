module B = Digestif_bigstring

(* XXX(dinosaure): don't use any external stub in this module. *)

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
  do set dst (dst_off + i) (B.get src (src_off + i)) done

let rpad a size x =
  let l = length a
  and b = create size in
  blit a 0 b 0 l;
  fill b l (size - l) x;
  b
