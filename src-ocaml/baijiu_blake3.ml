(* Digestif-specific pure OCaml implementation, not vendored upstream code.
   See src-c/native/blake3.c for the pinned C-source provenance. *)

module By = Digestif_by
module Bi = Digestif_bi

module Int32 = struct
  include Int32

  let ( lsl ) = shift_left
  let ( lsr ) = shift_right_logical
  let ( lor ) = logor
  let ( lxor ) = logxor
  let ( + ) = add
  let ror x n = (x lsr n) lor (x lsl (32 - n))
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

let iv =
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
  input_cv : int32 array;
  block : By.t;
  block_len : int;
  counter : int64;
  flags : int;
}

type chunk_state = {
  cv : int32 array;
  chunk_counter : int64;
  buf : By.t;
  mutable buf_len : int;
  mutable blocks_compressed : int;
  flags : int;
}

type ctx = {
  key : int32 array;
  mutable chunk : chunk_state;
  mutable cv_stack : int32 array list;
  flags : int;
}

let words_of_bytes bytes = Array.init 8 (fun i -> By.le32_to_cpu bytes (i * 4))

let bytes_of_words words =
  let bytes = By.create out_len in
  for i = 0 to 7 do
    By.cpu_to_le32 bytes (i * 4) words.(i)
  done ;
  bytes

let g state a b c d x y =
  let open Int32 in
  state.(a) <- state.(a) + state.(b) + x ;
  state.(d) <- ror (state.(d) lxor state.(a)) 16 ;
  state.(c) <- state.(c) + state.(d) ;
  state.(b) <- ror (state.(b) lxor state.(c)) 12 ;
  state.(a) <- state.(a) + state.(b) + y ;
  state.(d) <- ror (state.(d) lxor state.(a)) 8 ;
  state.(c) <- state.(c) + state.(d) ;
  state.(b) <- ror (state.(b) lxor state.(c)) 7

let round state msg round =
  let s = schedule.(round) in
  g state 0 4 8 12 msg.(s.(0)) msg.(s.(1)) ;
  g state 1 5 9 13 msg.(s.(2)) msg.(s.(3)) ;
  g state 2 6 10 14 msg.(s.(4)) msg.(s.(5)) ;
  g state 3 7 11 15 msg.(s.(6)) msg.(s.(7)) ;
  g state 0 5 10 15 msg.(s.(8)) msg.(s.(9)) ;
  g state 1 6 11 12 msg.(s.(10)) msg.(s.(11)) ;
  g state 2 7 8 13 msg.(s.(12)) msg.(s.(13)) ;
  g state 3 4 9 14 msg.(s.(14)) msg.(s.(15))

let compress cv block block_len counter flags =
  let msg = Array.init 16 (fun i -> By.le32_to_cpu block (i * 4)) in
  let state = Array.make 16 0l in
  Array.blit cv 0 state 0 8 ;
  Array.blit iv 0 state 8 4 ;
  state.(12) <- Int64.to_int32 counter ;
  state.(13) <- Int64.(to_int32 (shift_right_logical counter 32)) ;
  state.(14) <- Int32.of_int block_len ;
  state.(15) <- Int32.of_int flags ;
  for r = 0 to 6 do
    round state msg r
  done ;
  for i = 0 to 7 do
    state.(i) <- Int32.(state.(i) lxor state.(Stdlib.(i + 8))) ;
    state.(Stdlib.(i + 8)) <- Int32.(state.(Stdlib.(i + 8)) lxor cv.(i))
  done ;
  state

let output_chaining_value output =
  Array.sub
    (compress output.input_cv output.block output.block_len output.counter
       output.flags)
    0 8

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
      By.cpu_to_le32 wide (i * 4) words.(i)
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
    cv = Array.copy key;
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
      Array.blit words 0 state.cv 0 8 ;
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
    input_cv = Array.copy state.cv;
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
    input_cv = Array.copy key;
    block;
    block_len;
    counter = 0L;
    flags = flags lor parent;
  }

let parent_cv key flags left right =
  output_chaining_value (parent_output key flags left right)

let init_base key flags =
  {
    key = Array.copy key;
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
    cv = Array.copy state.cv;
    chunk_counter = state.chunk_counter;
    buf = By.copy state.buf;
    buf_len = state.buf_len;
    blocks_compressed = state.blocks_compressed;
    flags = state.flags;
  }

let dup ctx =
  {
    key = Array.copy ctx.key;
    chunk = dup_chunk ctx.chunk;
    cv_stack = List.map Array.copy ctx.cv_stack;
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
