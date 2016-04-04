
type t = 
  | Unterminated_string 
  | Internal
;;

exception Error of t;;

