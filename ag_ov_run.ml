let validate_array f a =
  let rec loop f a len i =
    if i >= len then true
    else
      if f a.(i) then loop f a len (i+1)
      else false
  in
  loop f a (Array.length a) 0

let validate_option f = function
    None -> true
  | Some x -> f x
