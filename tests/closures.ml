let rec f x = 
  (let rec g y = 
    (let rec h z =
      x + y + z in h)
    in g)
  in print_int (((f 1) 2) 3)
