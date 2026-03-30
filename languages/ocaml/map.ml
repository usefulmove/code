let rec mmap f lst = 
    match lst with
    | [] -> []
    | head :: rest -> f head :: mmap f rest

let cube n = n * n * n

let print_int_list lst =
  List.iter (fun x -> print_int x; print_string " ") lst;
  print_newline ()

let () = print_int_list (mmap cube [8; 1; 2])
