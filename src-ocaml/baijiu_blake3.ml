(* Digestif-specific pure OCaml implementation, not vendored upstream code.
   See src-c/native/blake3.c for the pinned C-source provenance. *)

module By = Digestif_by
module Bi = Digestif_bi

module Int32 = struct
  include Int32

  external ( lsl ) : int32 -> int -> int32 = "%int32_lsl"
  external ( lsr ) : int32 -> int -> int32 = "%int32_lsr"
  external ( lor ) : int32 -> int32 -> int32 = "%int32_or"
  external ( lxor ) : int32 -> int32 -> int32 = "%int32_xor"
  external ( + ) : int32 -> int32 -> int32 = "%int32_add"

  let[@inline] ror x n = (x lsr n) lor (x lsl (32 - n))
end

let block_len = 64
let chunk_len = 1024
let out_len = 32
let key_len = 32
let chunk_start = 1
let chunk_end = 2
let parent = 4
let root = 8
let keyed_hash = 16
let derive_key_context = 32
let derive_key_material = 64

let of_array a =
  let b = By.create (4 * Array.length a) in
  Array.iteri (fun i x -> By.unsafe_set_32 b (i * 4) x) a ;
  b

let iv =
  of_array
    [|
      0x6a09e667l; 0xbb67ae85l; 0x3c6ef372l; 0xa54ff53al; 0x510e527fl;
      0x9b05688cl; 0x1f83d9abl; 0x5be0cd19l;
    |]

let schedule =
  [|
    [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15 |];
    [| 2; 6; 3; 10; 7; 0; 4; 13; 1; 11; 12; 5; 9; 14; 15; 8 |];
    [| 3; 4; 10; 12; 13; 2; 7; 14; 6; 5; 9; 0; 11; 15; 8; 1 |];
    [| 10; 7; 12; 9; 14; 3; 13; 15; 4; 0; 11; 2; 5; 8; 1; 6 |];
    [| 12; 13; 9; 11; 15; 10; 14; 8; 7; 2; 5; 3; 0; 1; 6; 4 |];
    [| 9; 14; 11; 5; 8; 12; 15; 1; 13; 3; 0; 10; 2; 6; 4; 7 |];
    [| 11; 15; 5; 0; 1; 9; 8; 6; 14; 10; 2; 12; 3; 4; 7; 13 |];
  |]

type output = {
  input_cv : By.t;
  block : By.t;
  block_len : int;
  counter : int64;
  flags : int;
}

type chunk_state = {
  cv : By.t;
  chunk_counter : int64;
  buf : By.t;
  mutable buf_len : int;
  mutable blocks_compressed : int;
  flags : int;
}

type ctx = {
  key : By.t;
  mutable chunk : chunk_state;
  mutable cv_stack : By.t list;
  flags : int;
}

let words_of_bytes bytes =
  let words = By.create out_len in
  for i = 0 to 7 do
    By.unsafe_set_32 words (i * 4) (By.le32_to_cpu bytes (i * 4))
  done ;
  words

let bytes_of_words words =
  let bytes = By.create out_len in
  for i = 0 to 7 do
    By.cpu_to_le32 bytes (i * 4) (By.unsafe_get_32 words (i * 4))
  done ;
  bytes

let[@inline] g v m s i a b c d =
  let a = a * 4 and b = b * 4 and c = c * 4 and d = d * 4 in
  let m0 = By.unsafe_get_32 m (s.(2 * i) * 4) in
  let m1 = By.unsafe_get_32 m (s.((2 * i) + 1) * 4) in
  let open Int32 in
  let va = By.unsafe_get_32 v a + By.unsafe_get_32 v b + m0 in
  let vd = ror (By.unsafe_get_32 v d lxor va) 16 in
  let vc = By.unsafe_get_32 v c + vd in
  let vb = ror (By.unsafe_get_32 v b lxor vc) 12 in
  let va = va + vb + m1 in
  let vd = ror (vd lxor va) 8 in
  let vc = vc + vd in
  let vb = ror (vb lxor vc) 7 in
  By.unsafe_set_32 v a va ;
  By.unsafe_set_32 v b vb ;
  By.unsafe_set_32 v c vc ;
  By.unsafe_set_32 v d vd

let compress cv block block_len counter flags =
  let m = By.create 64 in
  for i = 0 to 15 do
    By.unsafe_set_32 m (i * 4) (By.le32_to_cpu block (i * 4))
  done ;
  let v = By.create 64 in
  By.blit cv 0 v 0 32 ;
  By.blit iv 0 v 32 16 ;
  By.unsafe_set_32 v 48 (Int64.to_int32 counter) ;
  By.unsafe_set_32 v 52 Int64.(to_int32 (shift_right_logical counter 32)) ;
  By.unsafe_set_32 v 56 (Int32.of_int block_len) ;
  By.unsafe_set_32 v 60 (Int32.of_int flags) ;
  for r = 0 to 6 do
    let s = schedule.(r) in
    g v m s 0 0 4 8 12 ;
    g v m s 1 1 5 9 13 ;
    g v m s 2 2 6 10 14 ;
    g v m s 3 3 7 11 15 ;
    g v m s 4 0 5 10 15 ;
    g v m s 5 1 6 11 12 ;
    g v m s 6 2 7 8 13 ;
    g v m s 7 3 4 9 14
  done ;
  for i = 0 to 7 do
    let x = By.unsafe_get_32 v (i * 4) in
    let y = By.unsafe_get_32 v ((i + 8) * 4) in
    By.unsafe_set_32 v (i * 4) Int32.(x lxor y) ;
    By.unsafe_set_32 v ((i + 8) * 4) Int32.(y lxor By.unsafe_get_32 cv (i * 4))
  done ;
  v

let output_chaining_value output =
  By.sub
    (compress output.input_cv output.block output.block_len output.counter
       output.flags)
    0 out_len

let output_root_bytes output ~seek dst off len =
  let output_block = ref Int64.(div seek 64L) in
  let within = ref Int64.(to_int (rem seek 64L)) in
  let dst_off = ref off in
  let remaining = ref len in
  let wide = By.create 64 in
  while !remaining > 0 do
    let words =
      compress output.input_cv output.block output.block_len !output_block
        (output.flags lor root) in
    for i = 0 to 15 do
      By.cpu_to_le32 wide (i * 4) (By.unsafe_get_32 words (i * 4))
    done ;
    let available = 64 - !within in
    let take = min !remaining available in
    By.blit wide !within dst !dst_off take ;
    dst_off := !dst_off + take ;
    remaining := !remaining - take ;
    output_block := Int64.succ !output_block ;
    within := 0
  done

let make_chunk_state key chunk_counter flags =
  {
    cv = By.copy key;
    chunk_counter;
    buf = By.make block_len '\x00';
    buf_len = 0;
    blocks_compressed = 0;
    flags;
  }

let chunk_state_len state =
  (block_len * state.blocks_compressed) + state.buf_len

let chunk_start_flag state =
  if state.blocks_compressed = 0 then chunk_start else 0

let chunk_state_update ~blit state input off len =
  let input_off = ref off in
  let remaining = ref len in
  while !remaining > 0 do
    if state.buf_len = block_len
    then (
      let words =
        compress state.cv state.buf block_len state.chunk_counter
          (state.flags lor chunk_start_flag state) in
      By.blit words 0 state.cv 0 out_len ;
      state.blocks_compressed <- state.blocks_compressed + 1 ;
      state.buf_len <- 0 ;
      By.fill state.buf 0 block_len '\x00') ;
    let take = min !remaining (block_len - state.buf_len) in
    blit input !input_off state.buf state.buf_len take ;
    state.buf_len <- state.buf_len + take ;
    input_off := !input_off + take ;
    remaining := !remaining - take
  done

let chunk_state_output state =
  {
    input_cv = By.copy state.cv;
    block = By.copy state.buf;
    block_len = state.buf_len;
    counter = state.chunk_counter;
    flags = state.flags lor chunk_start_flag state lor chunk_end;
  }

let parent_output key flags left right =
  let block = By.create block_len in
  let left = bytes_of_words left and right = bytes_of_words right in
  By.blit left 0 block 0 out_len ;
  By.blit right 0 block out_len out_len ;
  {
    input_cv = By.copy key;
    block;
    block_len;
    counter = 0L;
    flags = flags lor parent;
  }

let parent_cv key flags left right =
  output_chaining_value (parent_output key flags left right)

let init_base key flags =
  {
    key = By.copy key;
    chunk = make_chunk_state key 0L flags;
    cv_stack = [];
    flags;
  }

let rec add_chunk_cv ctx new_cv total_chunks =
  if Int64.(logand total_chunks 1L = 0L)
  then
    match ctx.cv_stack with
    | left :: rest ->
        ctx.cv_stack <- rest ;
        add_chunk_cv ctx
          (parent_cv ctx.key ctx.flags left new_cv)
          Int64.(shift_right_logical total_chunks 1)
    | [] -> assert false
  else ctx.cv_stack <- new_cv :: ctx.cv_stack

let unsafe_feed ~blit ctx input off len =
  let input_off = ref off in
  let remaining = ref len in
  while !remaining > 0 do
    if chunk_state_len ctx.chunk = chunk_len
    then (
      let chunk_cv = output_chaining_value (chunk_state_output ctx.chunk) in
      let total_chunks = Int64.succ ctx.chunk.chunk_counter in
      add_chunk_cv ctx chunk_cv total_chunks ;
      ctx.chunk <- make_chunk_state ctx.key total_chunks ctx.flags) ;
    let take = min !remaining (chunk_len - chunk_state_len ctx.chunk) in
    chunk_state_update ~blit ctx.chunk input !input_off take ;
    input_off := !input_off + take ;
    remaining := !remaining - take
  done

let unsafe_feed_bytes = unsafe_feed ~blit:By.blit

let unsafe_feed_bigstring (ctx : ctx) (input : Bi.t) off len =
  unsafe_feed ~blit:By.blit_from_bigstring ctx input off len

let final_output ctx =
  let output = ref (chunk_state_output ctx.chunk) in
  List.iter
    (fun left ->
      output :=
        parent_output ctx.key ctx.flags left (output_chaining_value !output))
    ctx.cv_stack ;
  !output

let unsafe_get_xof ctx ~seek dst off len =
  output_root_bytes (final_output ctx) ~seek dst off len

let unsafe_get ctx =
  let result = By.create out_len in
  unsafe_get_xof ctx ~seek:0L result 0 out_len ;
  result

let dup_chunk state =
  {
    cv = By.copy state.cv;
    chunk_counter = state.chunk_counter;
    buf = By.copy state.buf;
    buf_len = state.buf_len;
    blocks_compressed = state.blocks_compressed;
    flags = state.flags;
  }

let dup ctx =
  {
    key = By.copy ctx.key;
    chunk = dup_chunk ctx.chunk;
    cv_stack = List.map By.copy ctx.cv_stack;
    flags = ctx.flags;
  }

let init () = init_base iv 0

let init_keyed key =
  if String.length key <> key_len
  then invalid_arg "BLAKE3 keyed mode requires a 32-byte key" ;
  init_base (words_of_bytes (By.unsafe_of_string key)) keyed_hash

let context_key context =
  let ctx = init_base iv derive_key_context in
  unsafe_feed_bytes ctx (By.unsafe_of_string context) 0 (String.length context) ;
  unsafe_get ctx |> words_of_bytes

let init_derive_key context =
  init_base (context_key context) derive_key_material
