val encrypt : string -> string

(** Take a SHA-1 digest and outputs an hexstring.
      case : Format printing. **)
val sha1_to_hexstring : 
  ?case:[< `Lower | `Upper > `Lower] -> string -> string

