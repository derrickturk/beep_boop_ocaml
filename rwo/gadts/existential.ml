type showable =
  | Show: 'a * ('a -> string) -> showable

let show (Show (x, f)) = f x
