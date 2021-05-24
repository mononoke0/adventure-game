open Syntax
open World_syntax

(* 扉の状態を表す型 *)
type door_state_t = Locked | Open | Closed
(* あひるの状態を表す型*)
type ahiru_clean_state_t =  Open | Closed
type ahiru_cute_state_t =  Open | Closed
type ahiru_power_state_t =  Open | Closed

(* ゲームの状態（世界）を表す型 *)
type state_t = {
  mutable place : string;				 (* 現在、いる場所 *)
  mutable items : string list;				 (* 持ち物リスト *)
	  place_state : (string * string list ref) list; (* 各場所にある物 *)
  mutable door_state : door_state_t;			 (* ドアの状態 *)
  mutable hp1 : int;	
  mutable hp2 : int;
  mutable hp3 : int;		
  mutable hp4 : int; 
  mutable hp5 : int;   	(* ヒットポイント（不使用） *)
  mutable ahiru_clean_state : ahiru_clean_state_t; (*あひる状態*)    (** **)
  mutable ahiru_cute_state : ahiru_cute_state_t; (*あひる状態*)    (** **)
  mutable ahiru_power_state : ahiru_power_state_t; (*あひる状態*)    (** **)
}

(* 目的：移動コマンドを処理する *)
(* idou : state_t -> string -> chizu_list -> unit *)
let idou state houkou chizu_list =
  if (houkou = "入" || houkou = "出") && state.door_state <> Open then
    print_endline "扉が閉まっているよ。"
  else try
    let new_place = List.assoc houkou (List.assoc state.place chizu_list) in
    state.place <- new_place
  with Not_found -> print_endline "そこには行かれないよ。"

(* 以下、動作を処理する関数群 *)

(* 目的：メッセージを表示する *)
(* message : string -> state_t -> unit *)
let message str state = print_endline str

(* 目的：「取る」を処理する *)
(* toru : string -> state_t -> unit *)
let toru item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("ここに" ^ item ^ "はないよ。")
  else if List.mem item state.items
    then print_endline ("あなたはすでに" ^ item ^ "を持っているよ。")
  else (state.items <- item :: state.items;
  if state.place = "原っぱ" then print_endline ("あなたは" ^ item ^ "を手に入れた！")
  else r := List.filter (fun i -> i <> item) !r;
  if state.place = "みずうみ" then 
  (state.hp5 <- 1;
   print_endline( item ^ "を助けてくれてありがとう。赤ちゃんは目をつぶって元気がないよ..."))
	else print_endline ("あなたは" ^ item ^ "を手に入れた！"))

(* 目的：「置く」を処理する *)
(* oku : string -> state_t -> unit *)
let oku item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item state.items)
    then print_endline ("あなたは" ^ item ^ "を持っていないよ。")
  else (state.items <- List.filter (fun x -> x <> item) state.items;
	r := item :: !r;
	print_endline ("あなたは" ^ item ^ "を置いたよ。"))



let hiraku item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("ここに" ^ item ^ "はないよ。")
  else match state.door_state with
      Locked -> if state.hp1 = 1 && state.hp2 = 1 && state.hp3 = 1
          then (state.door_state <- Open;
          print_endline ("どうくつのとびらが開いたよ。"))
          else print_endline ("赤ちゃんの元気が回復しないと入れないよ。")
    | Open   -> print_endline ("とびらはすでに開いているよ。")
    | Closed -> (state.door_state <- Open;
           print_endline ("どうくつのとびらが開いたよ。"))


          

(* 目的：「閉じる」を処理する *)
(* tojiru : string -> state_t -> unit *)
let tojiru item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem item !r)
    then print_endline ("ここに" ^ item ^ "はないよ。")
  else match state.door_state with
     Locked -> print_endline ("扉はすでに閉まっているよ。")
    |Open   -> state.door_state <- Closed;
	        print_endline ("どうくつのとびらを閉めたよ。")
    | Closed -> print_endline ("とびらはすでに閉まっているよ。") 

(* 目的：「洗う」を処理する　*)
(*arau : string -> state_t -> unit *)                      
let arau item state =
  let r = List.assoc state.place state.place_state  in
  if ((not(List.mem item !r)) && (not(List.mem item state.items)))   
    then print_endline ("ここに" ^ item ^ "はいないよ。")
  else match state.ahiru_clean_state with
      Closed -> if List.mem "せっけん" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
                then (state.ahiru_clean_state <- Open;
               
                state.hp1 <- 1;
                
                print_endline ("赤ちゃんはピカピカになってごきげんみたい。"))
                else print_endline ("せっけんと赤ちゃんがいないと洗えないよ。")
    | Open   -> print_endline ("すでに赤ちゃんはピカピカだよ。")
    

let ireru item state =
  let r = List.assoc state.place state.place_state  in
  if ((not(List.mem item !r)) && (not(List.mem item state.items)))   
    then print_endline ("ここに" ^ item ^ "はいないよ。")
  else match state.ahiru_clean_state with
      Closed -> if List.mem "せっけん" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
                then (state.ahiru_clean_state <- Open;
               
                state.hp1 <- 1;
                
                print_endline ("赤ちゃんはピカピカになってごきげんみたい。"))
                else print_endline ("せっけんと赤ちゃんがいないと洗えないよ。")
    | Open   -> print_endline ("すでに赤ちゃんはピカピカだよ。")
    
(* 目的：「付ける」を処理する　*)
(* tsukeru : string -> state_t -> unit *)     
let tsukeru item state =
  let r = List.assoc state.place state.place_state  in 
  if ((not(List.mem item !r)) && (not(List.mem item state.items)))  
    then print_endline ("ここに" ^ item ^ "はないよ。")
  else match state.ahiru_cute_state with
      Closed -> if List.mem "リボン" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
                then (state.ahiru_cute_state <- Open;                 
                state.hp2 <- 1; 
                print_endline ("赤ちゃんはリボンをつけて笑顔になったよ。"))
                else print_endline ("リボンとつけてあげる赤ちゃんが必要だよ。")
    | Open   -> print_endline ("すでに赤ちゃんはリボンをつけているよ。")
  
(* 目的：「渡す」を処理する　*)
(* watasu : string -> state_t -> unit *)  
 
let watasu item state =
  if state.place = "野菜ばたけ" then
  let r = List.assoc state.place state.place_state  in 
  if ((not(List.mem item !r)) && (not(List.mem item state.items))) 
    then  print_endline ("ここに" ^ item ^ "はないよ。")
  else match state.ahiru_power_state with
      Closed -> if List.mem "絵本" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
          then (state.ahiru_power_state <- Open;         
          state.hp3 <- 1;            
          print_endline ("赤ちゃんは絵本を読んでもらって元気になったよ。"))
          else print_endline ("絵本と赤ちゃんが必要だよ。")
    | Open   -> print_endline ("すでに赤ちゃんは絵本を読んでもらったよ。")
  
  else if state.place = "教会" then
  let r = List.assoc state.place state.place_state  in  
  if ((not(List.mem item !r))&& (not(List.mem item state.items))) 
    then print_endline ("ここに" ^ item ^ "はありません。")
  else match state.ahiru_clean_state with
      Closed -> if List.mem "せっけん" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
                then (state.ahiru_clean_state <- Open;               
                state.hp1 <- 1;                         
                print_endline ("赤ちゃんはピカピカになってごきげんみたい。"))
                else print_endline ("せっけんと赤ちゃんがいないと洗えないよ。")
    | Open   -> print_endline ("すでに赤ちゃんはピカピカだよ。")
  
  else if state.place = "小屋" then
  let r = List.assoc state.place state.place_state  in  
  if ((not(List.mem item !r))&& (not(List.mem item state.items))) 
    then print_endline ("ここに" ^ item ^ "はないよ。")
  else match state.ahiru_cute_state with
      Closed -> if List.mem "リボン" state.items && List.mem "灰色のあひるの赤ちゃん" state.items
                then (state.ahiru_cute_state <- Open;             
                state.hp2 <- 1;                      
                print_endline ("赤ちゃんはリボンをつけて笑顔になったよ。"))
                else print_endline ("リボンとつけてあげる赤ちゃんが必要だよ。")
    | Open   -> print_endline ("すでに赤ちゃんはリボンをつけているよ。")
 
  else if state.place = "どうくつの中" then
  let r = List.assoc state.place state.place_state in
  if ((not(List.mem "たくさんの星" !r))&& (not(List.mem "たくさんの星" state.items))) 
    then print_endline ("あなたは目印になるものを持っていないよ。")         
  else (
  state.hp4 <- 1; 
  print_endline ("赤ちゃんは星をめざして仲間のあひるのもとへ飛んでいくことができたよ。ありがとう。赤ちゃんからお手紙があるよ。");
  state.items <- "手紙" :: state.items)
  

let yomu item state =
  let r = List.assoc state.place state.place_state in
  if not (List.mem "手紙" state.items)
    then print_endline ("あなたはまだお手紙を持っていないよ。")
         
  else (state.items <- List.filter (fun x -> x <> item) state.items;
  r := item :: !r;
  print_endline ("ありがとう。やさしい君のおかげできょうだいのもとにかえれるよ。ぼくも君のようなすてきなおにいさんになれるようにがんばるね。またかならず会いにくるからまっててね。あひるのぽっぽより");
  state.items <- "ありがとう" :: state.items)



(* 目的：入力文に従って動作を行う *)
(* dispatch : Syntax.t -> state_t -> dousa_list -> chizu_list -> unit *)
let dispatch input state dousa_list chizu_list = match input with
    Idousuru (houkou) -> idou state houkou chizu_list
  | Tadoushi (mokutekigo, tadoushi) ->
      let lst = List.assoc mokutekigo dousa_list in
		(* この目的語に使える動作のリストを得る *)
      (try
	 let thunk = List.assoc tadoushi lst in
	 thunk state (* 動作を実行 *)
       with Not_found ->
	     print_endline (mokutekigo ^ "を" ^
			    tadoushi ^ "ことはできないよ。"))
  | Tandokudoushi ("終了する") ->
      print_endline "また遊んでね！"; exit 0
  | Tandokudoushi (tandokudoushi) ->
      print_endline (tandokudoushi ^ "ことはできないよ。")

(* 目的：現在地の情報を表示する *)
(* basho_message : state_t -> unit *)
let basho_message state =
  print_endline ("あなたは" ^ state.place ^ "にいるよ。");
  print_string "ここには";
  match !(List.assoc state.place state.place_state) with
      [] -> print_endline "何もないみたい。";
      if state.place = "みずうみ" && state.hp5 = 0 then
      print_endline "あひるの赤ちゃんを仲間のあひるもとに返すお手伝いをしよう。"  (** **)
      else if state.place = "どうくつの中" && state.hp4 = 0 then
      print_endline "白鳥のお姉さんが一緒に飛ぶ練習をしてくれるよ。赤ちゃんは帰り道の目印を持っているかな..."     (** **)
      else if state.place = "小屋" && state.hp2 = 0 then
      print_endline "きらきらしたお姉さんがいるよ。赤ちゃんをおめかししてくれるかも"      (** **)
      else if state.place = "野菜ばたけ" && state.hp3 = 0 then
      print_endline "やさしいおじいさんがいるよ。おじいさんは絵本の読み聞かせが大すきみたい。"      (** **)
      else if state.place = "教会" && state.hp1 = 0 then
      (state.items <- "お風呂" :: state.items;
      print_endline "元気なおばあさんがいるよ。おばあさんと一緒に赤ちゃんをお風呂に入れてあげよう。")     (** **)
      else if state.place = "お花ばたけ"  then
      print_endline "色々な色のお花が咲いているよ。赤ちゃんといっしょにおひるねしちゃった..." 
      else if state.place = "大きなみずうみ" then
      print_endline "あひるの赤ちゃんのきょうだいたちを発見!ここまで赤ちゃんは１人でこれるかな..."   
 

    | item :: rest ->
	print_string item;
	List.iter (fun item ->
		     print_string "と";
		     print_string item)
		  rest;
  if state.place = "みずうみ" then print_endline("がいるよ。")
  else print_endline "があるよ。";
	(*print_string state.place; *)
  if state.place = "みずうみ" && state.hp5 = 0 then
  print_endline "あひるの赤ちゃんを仲間のあひるのもとに返すお手伝いをしよう。"  (** **)
  else if state.place = "どうくつの中" && state.hp4 = 0 then
  print_endline "白鳥のお姉さんが一緒に飛ぶ練習をしてくれるよ。赤ちゃんは帰り道の目印を持っているかな..."
  else if state.place = "小屋" && state.hp2 = 0 then
  print_endline "きらきらしたお姉さんがいるよ。赤ちゃんをおめかししてくれるかも"      (** **)
  else if state.place = "野菜ばたけ" && state.hp3 = 0 then
  print_endline "やさしいおじいさんがいるよ。おじいさんは本の読み聞かせが大すきみたい。"      (** **)
  else if state.place = "教会" && state.hp1 = 0 then
  (state.items <- "お風呂" :: state.items;
  print_endline "元気なおばあさんがいるよ。おばあさんと一緒に赤ちゃんをお風呂に入れてあげよう。")     (** **)
  else if state.place = "お花ばたけ" then
  print_endline "色々な色のお花が咲いているよ。赤ちゃんといっしょにおひるねしちゃった..." 
  else if state.place = "大きなみずうみ" then
  print_endline "あひるの赤ちゃんのきょうだいたちを発見!ここまで赤ちゃんは１人でこれるかな..."   

  




(* 目的：ゲームのメインループ *)
(* loop : state_t -> ... -> 'a *)
let rec loop state dousa_list chizu_list
	     shuryo_basho shuryo_items shuryo_messages =
  if state.place = shuryo_basho &&	(* 終了場所にいて *)
     List.fold_right (fun item b -> List.mem item state.items && b)
		     shuryo_items true	(* 終了アイテムを全て持っていたら *)
  then (List.iter print_endline shuryo_messages; (* 終了メッセージを全部表示 *)
	exit 0);
  basho_message state;
  print_string "> ";
  let line = read_line () in			(* １行読み込み、*)
  (try						(* 字句解析、構文解析をし *)
     let input = Parser.start Lexer.token (Lexing.from_string line) in
     dispatch input state dousa_list chizu_list	(* 動作を処理する *)
   with Error (str) -> print_endline str
      | Not_found -> print_endline "えっ？"
      | Parsing.Parse_error -> print_endline "ええっ？");
  print_newline ();
  loop state dousa_list chizu_list shuryo_basho shuryo_items shuryo_messages

(* ゲームの開始 *)
let _ = try
  let world = World_parser.start World_lexer.token
				 (Lexing.from_channel (open_in "world.txt")) in
  let messages = extract_shoki_messages world in
  List.iter print_endline messages; (* 初期メッセージを表示 *)
  print_newline ();
  (* ゲームの初期状態 *)
  let init_state = {
    place = extract_shuppatsuten world;
    items = extract_shoki_items world;
    place_state = extract_place_state world;
    door_state = Locked;
    ahiru_clean_state = Closed;
    ahiru_cute_state = Closed;
    ahiru_power_state = Closed;
    hp1 = 0;
    hp2 = 0;
    hp3 = 0;
    hp4 = 0;
    hp5 = 0;
  } in
  (* アクションの対応表 *)
  let action_list = [
    ("取る", toru); ("置く", oku);  ("洗う", arau); ("付ける", tsukeru); ("渡す", watasu); ("開く", hiraku); ("閉じる", tojiru); ("読む",yomu); ("入れる", ireru);
  ] in
  (* 動作 *)
  let dousa_list = extract_dousa_list world action_list message in
  (* 地図 *)
  let chizu_list = extract_chizu_list world in
  (* 終了場所 *)
  let shuryo_basho = extract_shuryo_basho world in
  (* 終了アイテム *)
  let shuryo_items = extract_shuryo_items world in
  (* 終了メッセージ *)
  let shuryo_messages = extract_shuryo_messages world in
  loop init_state dousa_list chizu_list
       shuryo_basho shuryo_items shuryo_messages
with Sys_error (str) ->
  failwith "world.txt が見つからないよ..."
