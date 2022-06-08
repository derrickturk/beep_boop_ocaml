(* the error this generates makes no fucking sense *)

open Base

type doc = 
  | Head of string
  | Para of text_item list
  | Defn of string list_item list
and text_item =
  | Raw of string
  | Bold of text_item list
  | Enum of int list_item list
  | Quote of doc
and 'a list_item = { tag: 'a; text: text_item list }

module Folder = struct
  type 'a t =
    { doc: 'a t -> 'a -> doc -> 'a
    ; text_item: 'a t -> 'a -> text_item -> 'a
    ; list_item: 'b. 'a t -> 'a -> 'b list_item -> 'a
    }

  let default: 'a t =
    let rec doc self acc = function
      | Head _ -> acc
      | Para ts -> List.fold ts ~init:acc ~f:(self.text_item self)
      | Defn ls -> List.fold ls ~init:acc ~f:(self.list_item self)
    and text_item self acc = function
      | Raw _ -> acc
      | Bold ts -> List.fold ts ~init:acc ~f:(self.text_item self)
      | Enum ls -> List.fold ls ~init:acc ~f:(self.list_item self)
      | Quote d -> self.doc self acc d
    and list_item self acc { text; _ } =
      List.fold text ~init:acc ~f:(self.text_item self)
    in { doc; text_item; list_item }

  let fold_doc ({ doc; _ } as self) d = doc self d
  let fold_text_item ({ text_item; _ } as self) t = text_item self t
  let fold_list_item ({ list_item; _ } as self) l = list_item self l
end

let bold_counter =
  let rec text_item self acc t =
    let acc = Folder.default.text_item self acc t in
    match t with
      | Bold _ -> acc + 1
      | _ -> acc
  in { Folder.default with text_item }

let count_bold = Folder.fold_doc bold_counter 0
