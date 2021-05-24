%{
(* 補助的な変数、関数、型などの定義 *)
open Syntax
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token <string> HOUKOU ITEM TADOUSHI TANDOKUDOUSHI
/* これらには string 型の値が伴うことを示している */
%token SOTO NAKA HE NI KARA SUSUMU HAIRU DERU WO
%token EOL
/* EOL = End Of Line 入力の終わり */

/* 非終端記号の型をここで宣言する */
%type <Syntax.t> start

/* 開始記号の定義 */
%start start


/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| bun EOL
        { $1 }
| bun anys EOL
        { raise (Error ("「" ^ $2 ^ "」？")) }

bun:
| HOUKOU houkoujoshi SUSUMU
        { Idousuru ($1) }
| HOUKOU houkoujoshi
        { raise (Error ("「" ^ $1 ^ "に」どうする？")) }
| HOUKOU
        { raise (Error ("「" ^ $1 ^ "」にどうする？")) }
| NAKA houkoujoshi HAIRU
        { Idousuru ("入") }
|SOTO NI DERU
        { Idousuru ("出")}
| SOTO houkoujoshi
        { raise (Error ("「どうくつの外に」どうする？")) }
| SOTO KARA DERU
        { Idousuru ("出") }
| SOTO KARA
        { raise (Error ("「どうくつの外から」どうする？")) }
| SOTO
        { raise (Error ("「どうくつの外」にどうする？")) }
| NAKA KARA DERU
        { Idousuru ("出") }
| NAKA KARA
        { raise (Error ("「どうくつの中から」どうする？")) }
| NAKA
        { raise (Error ("「どうくつの中」にどうする？")) }
| mokutekigo TADOUSHI
        { Tadoushi ($1, $2) }
| mokutekigo
        { raise (Error ("「" ^ $1 ^ "を」どうする？")) }
| TANDOKUDOUSHI
        { Tandokudoushi ($1) }
|
        { raise (Error ("え？")) }

houkoujoshi:
| HE
        { () (* 何も返す必要がない *) }
| NI
        { () (* 何も返す必要がない *) }

mokutekigo:
| ITEM WO
        { $1 }
| ITEM NI
        { $1 }
| ITEM
        { raise (Error ("「" ^ $1 ^ "」をどうする？")) }

anys:
| any           { $1 }
| any anys      { $1 ^ $2 }

any:
| HOUKOU        { $1 (* 返す文字列は、エラーメッセージ用 *) }
| SOTO          { "外" }
| NAKA          { "中" }
| HE            { "へ" }
| NI            { "に" }
| KARA          { "から" }
| SUSUMU        { "進む" }
| HAIRU         { "入る" }
| DERU          { "出る" }
| WO            { "を" }
| ITEM          { $1 }
| TADOUSHI      { $1 }
| TANDOKUDOUSHI { $1 }
