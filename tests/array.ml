let memo = Array.make 31 0 in
let rec fib x =
  if memo.(x) <> 0 then memo.(x)
  else 
  let ans = if x <= 1 then x else fib (x-1) + fib (x-2) in
  memo.(x) <- ans; ans
  in
print_int (fib 30)