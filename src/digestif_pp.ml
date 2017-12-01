module type B =
sig
  type t

  val create : int -> t
  val iter : (char -> unit) -> t -> unit
  val set : t -> int -> char -> unit
  val get : t -> int -> char
end

module type D =
sig
  val digest_size : int
end

module Make (S : B) (D : D) = struct
  let to_hex hash =
    let res = S.create (D.digest_size * 2) in

    let chr x = match x with
      | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 -> Char.chr (48 + x)
      | _ -> Char.chr (97 + (x - 10))
      (* XXX(dinosaure): I decided to use small character because we planned to
         used it in ocaml-git. So, this expected result is close to the Git
         format. A good idea is to put a new argument and specify the
         alphabet. *)
    in

    for i = 0 to D.digest_size - 1
    do
      let v = Char.code (S.get hash i) in
      S.set res (i * 2) (chr (v lsr 4));
      S.set res (i * 2 + 1) (chr (v land 0x0F));
    done;

    res

  let fold_s f a s =
    let r = ref a in
    S.iter (fun x -> r := f !r x) s; !r

  let of_hex hex =
    let code x = match x with
      | '0' .. '9' -> Char.code x - 48
      | 'A' .. 'F' -> Char.code x - 55
      | 'a' .. 'z' -> Char.code x - 87
      | _ -> raise (Invalid_argument "of_hex")
    in

    let wsp = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false in

    fold_s
      (fun (res, i, acc) -> function
         | chr when wsp chr -> (res, i, acc)
         | chr ->
           match acc, code chr with
           | None, x -> (res, i, Some (x lsl 4))
           | Some y, x -> S.set res i (Char.unsafe_chr (x lor y)); (res, succ i, None))
      (S.create D.digest_size, 0, None)
      hex
    |> function (_, _, Some _)  -> raise (Invalid_argument "of_hex")
              | (res, i, _) ->
                if i = D.digest_size
                then res
                else (for i = i to D.digest_size - 1 do S.set res i '\000' done; res)

  let pp fmt hash =
    for i = 0 to D.digest_size - 1
    do Format.fprintf fmt "%02x" (Char.code (S.get hash i)) done
end
