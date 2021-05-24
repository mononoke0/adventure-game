%{
(* 補助的な変数、関数、型などの定義 *)
open World_syntax
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token <string> SECTION SUBSECTION TEXT INDENTED_TEXT
/* これらには string 型の値が伴うことを示している */
%token SHOKIMESSAGE SHURYOUJOUKEN
%token EOF
/* EOF = End Of File 入力の終わり */

/* 非終端記号の型をここで宣言する */
%type <World_syntax.t> start

/* 開始記号の定義 */
%start start

/* 設定ファイル (world.txt) の構造：

設定ファイル = 初期部 セクション列 終了部

初期部       = "===初期メッセージ==="
               テキスト列

セクション   = "===セクション名==="
               ペア
               小セクション列

小セクション = "---小セクション名---"
               ペア列

終了部       = "===終了条件==="
               ペア列

ペア         = テキスト
               タブ付テキスト列

パーザの出力：World_syntax.t 型の値

*/

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

start:
| shoki_section sections shuryou_section EOF
        { { messages = $1;
            sections = $2;
            shuryo_jouken = $3; } }

shoki_section:
| SHOKIMESSAGE messages
        { $2 }

messages:
|
        { [] }
| TEXT messages
        { $1 :: $2 }

sections:
|
        { [] }
| section sections
        { $1 :: $2 }

section:
| SECTION pair subsections
        { ($1, { initial_pair = $2; subsections = $3 }) }

subsections:
|
        { [] }
| subsection subsections
        { $1 :: $2 }

subsection:
| SUBSECTION pairs
        { ($1, $2) }

pair:
| TEXT indented_texts
        { ($1, $2) }

pairs:
|
        { [] }
| pair pairs
        { $1 :: $2 }

indented_texts:
|
        { [] }
| INDENTED_TEXT indented_texts
        { $1 :: $2 }

shuryou_section:
| SHURYOUJOUKEN pairs
        { $2 }
