module Make (D : sig val digest_size : int end) = struct
  let to_hex hash =
    let res = Bytes.create (D.digest_size * 2) in

    let chr x = match x with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> Char.chr (48 + x)
      | _ -> Char.chr (97 + (x - 10)) in

    for i = 0 to D.digest_size - 1
    do
      let v = Char.code (String.get hash i) in
      Bytes.unsafe_set res (i * 2) (chr (v lsr 4));
      Bytes.unsafe_set res (i * 2 + 1) (chr (v land 0x0F));
    done;

    Bytes.unsafe_to_string res

  let fold f a s =
    let r = ref a in
    String.iter (fun x -> r := f !r x) s; !r

  let of_hex hex =
    let code x = match x with
      | '0' .. '9' -> Char.code x - 48
      | 'A' .. 'F' -> Char.code x - 55
      | 'a' .. 'z' -> Char.code x - 87
      | _ -> invalid_arg "of_hex" in

    let wsp = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in

    fold
      (fun (res, i, acc) -> function
         | chr when wsp chr -> (res, i, acc)
         | chr ->
           match acc, code chr with
           | None, x -> (res, i, Some (x lsl 4))
           | Some y, x -> Bytes.unsafe_set res i (Char.unsafe_chr (x lor y)); (res, succ i, None))
      (Bytes.create D.digest_size, 0, None)
      hex
    |> (function
        | (_, _, Some _)  -> invalid_arg "of_hex"
        | (res, i, _) ->
           if i = D.digest_size
           then res
           else (for i = i to D.digest_size - 1
                 do Bytes.unsafe_set res i '\000' done;
                 res))
    |> Bytes.unsafe_to_string

  let pp ppf hash =
    for i = 0 to D.digest_size - 1
    do Format.fprintf ppf "%02x" (Char.code (String.get hash i)) done
end
