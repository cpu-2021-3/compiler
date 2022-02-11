let pi = 3.1415926535 in
let rec floor x = 
  float_of_int (int_of_float (x -. 0.5))
in
let rec abs_float x = if x >= 0.0 then x else (0.0 -. x) in
let rec reduction_2pi x = 
  let p = Array.make 1 (pi *. 2.0) in
  let rec firstloop null = 
    if x >= p.(0) then 
      (p.(0) <- p.(0) *. 2.0; firstloop ())
    else () in
  firstloop ();
  let a = Array.make 1 (x) in
  let rec secondloop null =
    if a.(0) >= pi *. 2.0 then
      ((if a.(0) >= p.(0) then
        a.(0) <- a.(0) -. p.(0)
      else ());
      (p.(0) <- p.(0) *. 0.5);
      secondloop ())
    else () in
  secondloop ();
  a.(0)
  in
let rec kernel_sin x = 
  x 
  -. x *. x *. x /. 6.0 
  +. x *. x *. x *. x *. x /. 120.0
  -. x *. x *. x *. x *. x *. x *. x /. 5040.0
in
let rec kernel_cos x =
  1.0
  -. x *. x /. 2.0
  +. x *. x *. x *. x /. 24.0
  -. x *. x *. x *. x *. x *. x /. 720.0
in
let rec sin x =
  let flag1 = (if x >= 0.0 then 1.0 else -1.0) in
  let x = abs_float x in
  let x = reduction_2pi x in
  let (x, flag2) = if x >= pi then (x -. pi, -1.0) else (x, 1.0) in
  let x = if x >= pi *. 0.5 then pi -. x else x in
  let ans = 
    if x <= pi *. 0.25 then
      kernel_sin x
    else 
      kernel_cos (pi *. 0.5 -. x)
    in
  ans *. flag1 *. flag2
in
let rec cos x =
  let x = abs_float x in
  let x = reduction_2pi x in
  let (x, flag1) = if x >= pi then (x -. pi, -1.0) else (x, 1.0) in
  let (x, flag2) = if x >= pi *. 0.5 then (pi -. x, -1.0) else (x, 1.0) in
  let ans = 
    if x <= pi *. 0.25 then
      kernel_cos x
    else 
      kernel_sin (pi *. 0.5 -. x)
    in
  ans *. flag1 *. flag2
in
let rec kernel_atan x =
  x
  -. x *. x *. x /. 3.0
  +. x *. x *. x *. x *. x /. 5.0
  -. x *. x *. x *. x *. x *. x *. x /. 7.0
  +. x *. x *. x *. x *. x *. x *. x *. x *. x /. 9.0
  -. x *. x *. x *. x *. x *. x *. x *. x *. x *. x *. x /. 11.0
  +. x *. x *. x *. x *. x *. x *. x *. x *. x *. x *. x *. x *. x /. 13.0
in
let rec atan x =
  let flag = if x >= 0.0 then 1.0 else -1.0 in
  let x = abs_float x in
  let ans = 
    if x < 0.4375 then 
      kernel_atan x
    else if x < 2.4375 then
      pi *. 0.25 +. kernel_atan ((x -. 1.0) /. (x +. 1.0))
    else
      pi *. 0.5 -. kernel_atan (1.0 /. x)
    in
  ans *. flag

  in

let rec print_newline null =
  print_byte 10
in
let rec print_int x =
  (if x < 0 then print_byte 45 else ());
  let x = if x >= 0 then x else 0 - x in
  let rec quot x y =
    if x < y then 0 else 1 + quot (x - y) y
  in
  let rec rem x y = 
    if x < y then x else rem (x - y) y
  in
  if x < 10 then
    print_byte (x + 48)
  else if x < 100 then
    (print_byte (quot x 10 + 48);
    print_byte (rem x 10 + 48))
  else 
    print_byte (quot x 100 + 48);
    let xx = rem x 100 in
    print_byte (quot xx 10 + 48);
    print_byte (rem xx 10 + 48)
  in
  let x = 
  (int_of_float
     ((sin (cos (sqrt (abs_float (-12.3))))
         +. 4.5 -. 6.7 *. 8.9 /. 1.23456789)
        *. float_of_int 1000000))
  in ()