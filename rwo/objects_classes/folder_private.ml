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
    | Head h -> self#head acc h
    | Para ts -> self#para acc ts
    | Defn ls -> self#defn acc ls

  method text_item acc = function
    | Raw s -> self#raw acc s
    | Bold ts -> self#bold acc ts
    | Enum ls -> self#enum acc ls
    | Quote d -> self#quote acc d

  method list_item: 'b. 'a -> 'b list_item -> 'a =
    fun acc { text; _ } -> List.fold text ~init:acc ~f:self#text_item

  method private head acc _ = acc
  method private para acc ts = List.fold ts ~init:acc ~f:self#text_item
  method private defn acc ls = List.fold ls ~init:acc ~f:self#list_item

  method private raw acc _ = acc
  method private bold acc ts = List.fold ts ~init:acc ~f:self#text_item
  method private enum acc ls = List.fold ls ~init:acc ~f:self#list_item
  method private quote acc d = self#doc acc d
end

class bold_counter = object
  inherit [int] folder as super
  (* n.b. we can remove "private" here and make this public *)
  method private bold acc ts = super#bold acc ts + 1
end

let count_bold = (new bold_counter)#doc 0

(* "really" hide private methods - now we can't use them via inheritance
 *   in subclasses of bold_counter_final *)
class bold_counter_final: object
  method doc: int -> doc -> int
  method list_item: int -> 'b list_item -> int
  method text_item: int -> text_item -> int
end = bold_counter
(* obviously we could also repeat the defn of bold_counter here *)
