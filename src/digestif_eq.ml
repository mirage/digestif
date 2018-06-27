module Make (D : sig val digest_size : int end) = struct
  let eq a b =
    let ret = ref 0 in
    for i = 0 to D.digest_size - 1
    do ret := !ret lor ((Char.code (String.get a i)) lxor (Char.code (String.get b i))) done;
    !ret <> 0

  let neq a b = not (eq a b)
end

