{
(* vim: set syntax=ocaml: *)
(* 補助的な変数、関数、型などの定義 *)
open World_parser
(* 以下を true にすると、world.txt の読み込めた部分まで表示される *)
let debug = true
}

(* 正規表現の略記 *)
let space = [' ' '\t']
let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
(* let zenkaku1 = ['\161'-'\254'] *)
              (* 0xA1 - 0xFE, EUC の１文字目 *)
let zenkaku1 = ['\224'-'\239']
              (* 0xE0 - 0xEF, UTF-8 の３バイト文字の１文字目 *)
let zenkaku = zenkaku1 _ _

rule token = parse
| "\n"
	{ token lexbuf }
| "===初期メッセージ===\n"
        { if debug then print_string (Lexing.lexeme lexbuf);
	  SHOKIMESSAGE }
| "===終了条件===\n"
        { if debug then print_string (Lexing.lexeme lexbuf);
	  SHURYOUJOUKEN }
| "===" zenkaku+ "===\n"
	{ let s = Lexing.lexeme lexbuf in
	  if debug then print_string s;
	  SECTION (String.sub s 3 (String.length s - 7)) }
| "---" zenkaku+ "---\n"
	{ let s = Lexing.lexeme lexbuf in
	  if debug then print_string s;
	  SUBSECTION (String.sub s 3 (String.length s - 7)) }
| zenkaku+ "\n"
	{ let s = Lexing.lexeme lexbuf in
	  if debug then print_string s;
	  TEXT (String.sub s 0 (String.length s - 1)) }
| "\t" zenkaku+ "\n"
	{ let s = Lexing.lexeme lexbuf in
	  if debug then print_string s;
	  INDENTED_TEXT (String.sub s 1 (String.length s - 2)) }
| eof
	{ EOF }
| ([^ '\n'] * as str) "\n" ?
	{ (* 字句解析できなかった部分をエラーとして例外を起こす *)
	  failwith ("unknown token:[" ^ str ^ "]") }
