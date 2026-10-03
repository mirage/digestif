(* Throughput of the pure OCaml cores, best of three over a calibrated count. *)

let floor_seconds = 0.5
let size = 1 lsl 20
let buf = String.init size (fun i -> Char.chr (i land 0xff))
let sink = ref 0
let now () = Unix.gettimeofday ()

let bench name digest =
  let once n =
    let t = now () in
    for _ = 1 to n do
      sink := !sink + String.length (digest buf)
    done;
    now () -. t
  in
  let n = ref 1 in
  while once !n < floor_seconds do n := !n * 2 done;
  let best = ref infinity in
  for _ = 1 to 3 do
    let t = once !n in
    if t < !best then best := t
  done;
  let per = !best /. float_of_int !n in
  Printf.printf "%-12s %8d %10.3f ms %9.1f MiB/s\n" name !n (per *. 1000.0)
    (float_of_int size /. per /. 1048576.0)

let () =
  Printf.printf "%-12s %8s %13s %14s\n" "hash" "iters" "per call" "throughput";
  bench "md5" (fun s -> Digestif.MD5.(to_raw_string (digest_string s)));
  bench "sha1" (fun s -> Digestif.SHA1.(to_raw_string (digest_string s)));
  bench "sha224" (fun s -> Digestif.SHA224.(to_raw_string (digest_string s)));
  bench "sha256" (fun s -> Digestif.SHA256.(to_raw_string (digest_string s)));
  bench "sha384" (fun s -> Digestif.SHA384.(to_raw_string (digest_string s)));
  bench "sha512" (fun s -> Digestif.SHA512.(to_raw_string (digest_string s)));
  bench "sha3_256" (fun s ->
      Digestif.SHA3_256.(to_raw_string (digest_string s)));
  bench "sha3_512" (fun s ->
      Digestif.SHA3_512.(to_raw_string (digest_string s)));
  bench "blake2b" (fun s -> Digestif.BLAKE2B.(to_raw_string (digest_string s)));
  bench "blake2s" (fun s -> Digestif.BLAKE2S.(to_raw_string (digest_string s)));
  bench "rmd160" (fun s -> Digestif.RMD160.(to_raw_string (digest_string s)));
  bench "whirlpool" (fun s ->
      Digestif.WHIRLPOOL.(to_raw_string (digest_string s)));
  if !sink = 0 then print_string "the sink never moved\n"
