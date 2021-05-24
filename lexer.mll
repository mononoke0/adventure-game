{
(* vim: set syntax=ocaml: *)
(* 補助的な変数、関数、型などの定義 *)
open Parser
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
| "東" | "ひがし"
	{ HOUKOU ("東") }
| "西" | "にし"
	{ HOUKOU ("西") }
| "南" | "みなみ"
	{ HOUKOU ("南") }
| "北" | "きた"
	{ HOUKOU ("北") }
| "どうくつの外" | "外" |"どうくつの前"
	{ SOTO }
| "どうくつの中" | "中" 
	{ NAKA }
| "へ"
	{ HE }
| "に"
	{ NI }
| "から"
	{ KARA }
| "進む" | "すすむ" | "行く" | "いく" | "向かう" | "むかう" | "歩く" | "あるく"
	{ SUSUMU }
| "入る" | "はいる"
	{ HAIRU }
| "出る" | "でる"
	{ DERU }
| "を"
	{ WO }

| "ドア" | "扉" | "とびら"
	{ ITEM ("とびら") }

|"リボン"
	{ITEM ("リボン")}
|"せっけん" |"石鹸" |"石けん"
	{ITEM ("せっけん")}
|"絵本" |"えほん" |"絵ほん" | "本"
	{ITEM ("絵本")}
|"赤ちゃん" |"あひるの赤ちゃん" |"灰色のあひる" |"あかちゃん" | "あひる" | "アヒル"
	{ITEM("灰色のあひるの赤ちゃん")}
|"たくさんの星" | "星" | "ほし" |"たくさんのほし" | "沢山の星"
	{ITEM("たくさんの星")}
|"手紙" | "お手紙" | "てがみ" | "おてがみ"
	{ITEM("手紙")}
|"お風呂" | "おふろ" | "ふろ" | "風呂"
	{ITEM ("お風呂")}
| "取る" | "とる" | "拾う" | "ひろう" | "掴む" | "つかむ"
	{ TADOUSHI ("取る") }
| "置く" | "おく"
	{ TADOUSHI ("置く") }
| "開く" | "ひらく" | "開ける" | "あける" | "さす" | "挿す"
	{ TADOUSHI ("開く") }
| "閉じる" | "とじる" | "閉める" | "しめる"
	{ TADOUSHI ("閉じる") }
| "入れる" | "いれる"
	{TADOUSHI ("入れる")}
| "洗う" | "あらう"
	{TADOUSHI ("洗う") }
| "付ける" | "つける"
	{TADOUSHI ("付ける") }
| "渡す" | "わたす"
	{TADOUSHI ("渡す") }
|"読む" | "よむ"
	{TADOUSHI ("読む")}
| "終了する" | "しゅうりょうする" | "終わる" | "おわる"
	{ TANDOKUDOUSHI ("終了する") }

| eof
	{ EOL }
| _
	{ (* 字句解析できなかった部分をエラーとして例外を起こす *)
	  let input = Bytes.to_string lexbuf.Lexing.lex_buffer in (* 入力全体 *)
	  let pos = Lexing.lexeme_start lexbuf in (* マッチした先頭の位置 *)
	  let str = String.sub input pos (String.length input - pos) in
	  raise (Syntax.Error ("「" ^ str ^ "」？")) }
