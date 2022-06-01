type 'a t
include (module type of Fqueue) with type 'a t := 'a t
include Foldable.FoldableExt with type 'a t := 'a t
