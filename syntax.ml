(* 字句解析、構文解析中のエラー *)
exception Error of string

(* Syntax.t : ユーザの入力文の型 *)
type t = Idousuru of string		(* 移動する *)
       | Tadoushi of string * string	(* 他動詞 *)
       | Tandokudoushi of string	(* 単独動詞 *)
