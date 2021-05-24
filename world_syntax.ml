(* (名前, 値のリスト) *)
type pair_t = string * string list

(* subsection (---名前---) の型 *)
type subsection_t = string * pair_t list

(* section (===名前===) の型 *)
type section_content_t = {
  initial_pair : pair_t;
  subsections : subsection_t list;
}

type section_t = string * section_content_t

(* world_parser が返す構文木の型 *)
type t = {
  messages : string list;
  sections : section_t list;
  shuryo_jouken : pair_t list;
}

(* 以下、World_syntax.t の値から各部の値を取り出す関数群 *)
let extract_shoki_messages { messages = ms } = ms

let extract_shuppatsuten { sections = lst } =
  let section = List.assoc "場所" lst in
  match section.initial_pair with
      ("スタート地点", [place]) -> place
    | ("スタート地点", _::_) ->
	failwith "スタート地点が１ヶ所以上、指定されています。"
    | _ ->
	failwith "スタート地点が指定されていません。"

let extract_shoki_items { sections = lst } =
  let section = List.assoc "アイテム" lst in
  match section.initial_pair with
      ("初期アイテム", items) -> items
    | _ -> []

let extract_chizu_list { sections = lst } =
  let section = List.assoc "場所" lst in
  List.map (fun (name, pairs) ->
	     (name,
	      let pairs2 = List.filter (fun (name, _) -> name <> "アイテム")
				       pairs in
	      List.map (function (name, []) ->
				   failwith (name ^ "の行き先がありません。")
			       | (name, [s]) -> (name, s)
			       | (name, _::_) ->
				   failwith (name ^ "の行き先が多過ぎます。"))
		       pairs2))
	   section.subsections

let extract_place_state { sections = lst } =
  let section = List.assoc "場所" lst in
  List.map (fun (name, pairs) ->
	      (name, ref (try
			    List.assoc "アイテム" pairs
			  with Not_found -> [])))
	   section.subsections

let extract_dousa_list { sections = lst } action_list message_fun =
  let section = List.assoc "アイテム" lst in
  List.map (fun (name, pairs) ->
	      (name,
	       List.map (function (n, []) ->
				    let action = List.assoc n action_list in
				    (n, action name)
			        | (n, [s]) -> (n, message_fun s)
			        | (n, _) ->
				    failwith "アクションが多過ぎます。")
			 pairs))
	   section.subsections

let extract_shuryo_basho { shuryo_jouken = lst } =
  try
    match List.assoc "場所" lst with
        [] -> failwith "終了場所が指定されていません。"
      | [basho] -> basho
      | _ :: _ -> failwith "終了場所が多過ぎます。"
  with Not_found -> failwith "終了場所が指定されていません。"

let extract_shuryo_items { shuryo_jouken = lst } =
  try
    List.assoc "アイテム" lst
  with Not_found -> failwith "終了アイテムが指定されていません。"

let extract_shuryo_messages { shuryo_jouken = lst } =
  try
    List.assoc "メッセージ" lst
  with Not_found -> failwith "終了メッセージが指定されていません。"
