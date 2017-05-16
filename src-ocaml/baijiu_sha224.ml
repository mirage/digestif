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
  module SHA256 = Baijiu_sha256.Make (B)

  include SHA256

  let init () =
    let b = B.create 128 in

    B.fill b 0 128 '\x00';

    { size = 0L
    ; b
    ; h = [| 0xc1059ed8l
           ; 0x367cd507l
           ; 0x3070dd17l
           ; 0xf70e5939l
           ; 0xffc00b31l
           ; 0x68581511l
           ; 0x64f98fa7l
           ; 0xbefa4fa4l |] }

  let get ctx =
    let res = get ctx in
    B.sub res 0 28
end


