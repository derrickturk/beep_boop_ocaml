(* oh hell yeah, this is the one place OO makes sense with ADTs *)

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

class ['a] folder = object (self)
  method doc acc = function
    | Head _ -> acc
    | Para ts -> List.fold ts ~init:acc ~f:self#text_item
    | Defn ls -> List.fold ls ~init:acc ~f:self#list_item

  method text_item acc = function
    | Raw _ -> acc
    | Bold ts -> List.fold ts ~init:acc ~f:self#text_item
    | Enum ls -> List.fold ls ~init:acc ~f:self#list_item
    | Quote d -> self#doc acc d

  method list_item: 'b. 'a -> 'b list_item -> 'a =
    fun acc { text; _ } -> List.fold text ~init:acc ~f:self#text_item
end

class bold_counter = object
  inherit [int] folder as super

  method text_item acc t =
    let acc = super#text_item acc t in
    match t with
      | Bold _ -> acc + 1
      | _ -> acc
end

let count_bold = (new bold_counter)#doc
