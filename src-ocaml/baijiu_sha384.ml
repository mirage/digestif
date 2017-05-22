module type S =
sig
  type t
  type ctx
  type buffer

  val init : unit -> ctx
  val feed : ctx -> t -> int -> int -> unit
  val get  : ctx -> t
end

module Make (B : Baijiu_buffer.S)
  : S with type buffer = B.buffer and type t = B.buffer
  = struct
  module SHA512 = Baijiu_sha512.Make (B)

  include SHA512

  let init () =
    let b = B.create 128 in

    B.fill b 0 128 '\x00';

    { size = [| 0L; 0L |]
    ; b
    ; h = [| 0xcbbb9d5dc1059ed8L
           ; 0x629a292a367cd507L
           ; 0x9159015a3070dd17L
           ; 0x152fecd8f70e5939L
           ; 0x67332667ffc00b31L
           ; 0x8eb44a8768581511L
           ; 0xdb0c2e0d64f98fa7L
           ; 0x47b5481dbefa4fa4L |] }

  let get ctx =
    let res = get ctx in
    B.sub res 0 48
end
