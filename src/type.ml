(**************************************************************************)
(*                                                                        *)
(*                                 OCC                                    *)
(*                                                                        *)
(**************************************************************************)
type inttype = 
  | Byte
  | UByte
  | Short
  | UShort
  | Int
  | UInt
  | Long
  | ULong
  | Long_Long
  | ULong_Long

type floattype =
  | Float
  | Double
  | Long_Double

type value =
  | Strval of string
  | Intval of int * inttype
  | Floatval of float * floattype
  | Charval of string
  | Runeval of int 
