
type t = 
  | Invalid_char
  | Invalid_rune
  | Invalid_sequence
  | Unterminated_string 
  | Internal
;;

exception Error of t;;

