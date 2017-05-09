open Bigarray

(* XXX(dinosaure): don't use any external stub in this module. *)

type t = (char, int8_unsigned_elt, c_layout) Array1.t
type v = { off : int
         ; len : int
         ; buf : t }

let length = Array1.dim
let create = Array1.create Char c_layout
let get    = Array1.get
let set    = Array1.set
let sub    = Array1.sub
let init l f =
  let v = create l in
  for i = 0 to l - 1 do set v i (f i) done;
  v
let fill x off len v =
  let x' = sub x off len in
  Array1.fill x' v
let copy v =
  let v' = create (length v) in
  Array1.blit v v'; v'

external get_u16 : t -> int -> int   = "%caml_bigstring_get16u"
external get_u32 : t -> int -> int32 = "%caml_bigstring_get32u"
external get_u64 : t -> int -> int64 = "%caml_bigstring_get64u"
external set_u16 : t -> int -> int -> unit   = "%caml_bigstring_set16u"
external set_u32 : t -> int -> int32 -> unit = "%caml_bigstring_set32u"
external set_u64 : t -> int -> int64 -> unit = "%caml_bigstring_set64u"

let to_string v =
  let buf = Bytes.create (length v) in
  for i = 0 to length v - 1
  do Bytes.set buf i (get v i) done;
  Bytes.unsafe_to_string buf

let blit src src_off dst dst_off len =
  let a = sub src src_off len in
  let b = sub dst dst_off len in

  Array1.blit a b

let blit_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do set dst (dst_off + i) (Bytes.get src (src_off + i)) done

let rpad a size x =
  let l = length a
  and b = create size in
  blit a 0 b 0 l;
  fill b l (size - l) x;
  b

exception Break of int

let compare a b =
  if length a <> length b
  then (if length a < length b then 0 - (Char.code (get b (length a))) else (Char.code (get a (length b))))
  else
    try
      for i = 0 to length a - 1
      do if get a i <> get b i then raise (Break ((Char.code (get a i)) - (Char.code (get b i)))) done;
      0
    with Break n -> n

let eq a b = compare a b = 0
let neq a b = not (eq a b)

let iter f x =
  let l = length x in

  for i = 0 to l - 1 do f (get x i) done

let empty = create 0

let pp fmt ba =
  for i = 0 to length ba - 1
  do Format.fprintf fmt "%c" (get ba i) done
