val encrypt : string -> string

(** Take a SHA-256 digest and outputs an hexstring.                                               
      case : Format printing. **)
val sha256_to_hexstring :
  ?case:[< `Lower | `Upper > `Lower] -> string -> string

