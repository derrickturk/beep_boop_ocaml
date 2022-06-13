let rec fix f x = f (fix f) x

let fac =
  let fac' f n = if n = 0 then 1 else n * f (n - 1) in
  fix fac'

let rec fix2 f =
  let rec a inp =
    let (fix_a, _) = f (fix2 f) in
    fix_a inp
  and b inp =
    let (_, fix_b) = f (fix2 f) in
    fix_b inp
  in (a, b)

let (even, odd) =
  let even' (e, o) x = if x = 0 then true else o (x - 1) in
  let odd' (e, o) x = if x = 0 then false else e (x - 1) in
  fix2 (fun fs -> (even' fs, odd' fs))

let rec fix3 f =
  let rec a inp =
    let (fix_a, _, _) = f (fix3 f) in
    fix_a inp
  and b inp =
    let (_, fix_b, _) = f (fix3 f) in
    fix_b inp
  and c inp =
    let (_, _, fix_c) = f (fix3 f) in
    fix_c inp
  in (a, b, c)

let (hodge, podge, modge) =
  let hodge' (h, p, m) x = if x = 0 then true else p (x - 1) in
  let podge' (h, p, m) x = if x = 0 then false else m (x - 1) in
  let modge' (h, p, m) x = if x = 0 then false else h (x - 1) in
  fix3 (fun fs -> (hodge' fs, podge' fs, modge' fs))
