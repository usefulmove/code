# Functional Programming in OCaml

OCaml is a statically-typed language in the ML family. FP is its primary paradigm: functions are first-class, values are immutable by default, and the type system (with algebraic data types and pattern matching) makes it natural to write correct, compositional code. The `|>` pipe operator and curried-by-default functions make pipelines feel native.

## Core FP Concepts in OCaml

### 1. Immutability

```ocaml
(* All values are immutable by default *)
let numbers = [1; 2; 3; 4; 5]
(* numbers <- [6; 7; 8]  -- Error! Can't rebind with <-  *)

(* Creating new lists instead of mutating *)
let extended = 0 :: numbers        (* [0; 1; 2; 3; 4; 5] *)
let combined = numbers @ [6; 7; 8] (* [1; 2; 3; 4; 5; 6; 7; 8] *)

(* Records are immutable by default *)
type point = { x: float; y: float }
let p = { x = 3.0; y = 4.0 }
(* p.x <- 5.0  -- Error! Field is not mutable *)

(* Functional update syntax creates a new record *)
let moved = { p with x = 10.0 }  (* { x = 10.0; y = 4.0 } *)

(* Mutable refs exist but are avoided in FP style *)
let counter = ref 0
counter := !counter + 1  (* Mutation via := and dereference via ! *)
```

### 2. Pure Functions

```ocaml
(* Pure function: same input -> same output, no side effects *)
let square x = x * x

(* Pure function with multiple arguments (curried by default) *)
let hypotenuse a b = sqrt (a *. a +. b *. b)  (* float -> float -> float *)

(* Not pure (uses a ref for side effects) *)
let call_count = ref 0
let impure_square x =
  incr call_count;  (* Side effect! *)
  x * x

(* Not pure (I/O is a side effect) *)
let impure_print_square x =
  let result = x * x in
  Printf.printf "%d\n" result;  (* Side effect! *)
  result
```

### 3. Higher-Order Functions (Functions as First-Class Objects)

```ocaml
(* Functions can be passed as arguments *)
let apply_to_all f lst = List.map f lst

(* Anonymous functions with fun keyword *)
let square = fun x -> x * x
let add = fun a b -> a + b

(* Concise function syntax *)
let square x = x * x

(* Using higher-order functions *)
let numbers = [1; 2; 3; 4; 5]
let squares = apply_to_all square numbers  (* [1; 4; 9; 16; 25] *)
let squares = apply_to_all (fun x -> x * x) numbers  (* same result *)

(* Functions returning functions (closures) *)
let multiplier factor = fun x -> x * factor

let double = multiplier 2
let triple = multiplier 3

let () =
  Printf.printf "%d\n" (double 5);  (* 10 *)
  Printf.printf "%d\n" (triple 5)   (* 15 *)
```

---

## Currying & Partial Application

OCaml functions are curried by default — every function takes exactly one argument and returns a function or a value. Multi-argument functions are syntactic sugar for nested single-argument functions.

```ocaml
(* These two definitions are identical *)
let add a b = a + b
let add = fun a -> fun b -> a + b

(* Partial application is free — just supply fewer arguments *)
let add5 = add 5          (* int -> int *)
let result = add5 3       (* 8 *)

(* Practical use: partially apply to create specialised functions *)
let double = List.map (fun x -> x * 2)
let numbers = [1; 2; 3; 4; 5]
let doubled = double numbers  (* [2; 4; 6; 8; 10] *)

(* Partial application with labeled arguments *)
let power ~base ~exp = Float.pow base exp

let square = power ~exp:2.0
let cube   = power ~exp:3.0

let () =
  Printf.printf "%g\n" (square ~base:5.0);  (* 25. *)
  Printf.printf "%g\n" (cube   ~base:3.0)   (* 27. *)
```

---

## Function Composition

```ocaml
(* |> is the pipe operator (left-to-right application) *)
let result =
  [1; 2; 3; 4; 5]
  |> List.filter (fun x -> x mod 2 = 0)   (* [2; 4] *)
  |> List.map (fun x -> x * x)            (* [4; 16] *)
  |> List.fold_left (+) 0                  (* 20 *)

(* @@ applies a function to a value (right-to-left, like $  in Haskell) *)
let result = string_of_int @@ List.length @@ List.filter (fun x -> x > 3) [1;2;3;4;5]
(* "2" *)

(* Manual compose: right-to-left *)
let compose f g x = f (g x)
let (%) f g x = f (g x)  (* Common operator alias *)

let square_then_string = string_of_int % (fun x -> x * x)
let () = Printf.printf "%s\n" (square_then_string 5)  (* "25" *)

(* Manual pipe: left-to-right compose *)
let pipe f g x = g (f x)
let (|>>) f g x = g (f x)

let process = (fun x -> x * 2) |>> (fun x -> x + 1) |>> string_of_int
let () = Printf.printf "%s\n" (process 5)  (* "11" *)

(* Useful combinators from the Fun module (OCaml 4.08+) *)
let _ = Fun.id 42           (* 42  — identity function *)
let _ = Fun.const 5 "hello" (* 5   — always returns first arg, ignores second *)

(* Fun.flip reverses the first two arguments of a binary function *)
let sub a b = a - b
let rsub = Fun.flip sub      (* rsub b a = sub a b *)
let _ = rsub 3 10            (* 10 - 3 = 7 *)

(* Practical: flip lets you partially apply the "data" argument first *)
let drop_first = Fun.flip List.nth 0  (* equivalent to List.hd *)
```

---

## List Operations

### map

```ocaml
let numbers = [1; 2; 3; 4; 5]

let squares = List.map (fun x -> x * x) numbers
(* [1; 4; 9; 16; 25] *)

let words = ["hello"; "world"]
let upper = List.map String.uppercase_ascii words
(* ["HELLO"; "WORLD"] *)

(* map over multiple lists with List.map2 *)
let a = [1; 2; 3]
let b = [10; 20; 30]
let sums = List.map2 (+) a b
(* [11; 22; 33] *)
```

### filter

```ocaml
let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let evens = List.filter (fun x -> x mod 2 = 0) numbers
(* [2; 4; 6; 8; 10] *)

let positives = List.filter (fun x -> x > 0) [-2; -1; 0; 1; 2]
(* [1; 2] *)

(* filter_map: filter and transform in one pass *)
let safe_sqrt x = if x >= 0.0 then Some (sqrt x) else None

let results = List.filter_map safe_sqrt [-1.0; 4.0; -9.0; 16.0]
(* [2.; 4.] *)
```

### partition

```ocaml
let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

(* Split into two lists in one pass — elements satisfying and not satisfying predicate *)
let (evens, odds) = List.partition (fun x -> x mod 2 = 0) numbers
(* evens = [2; 4; 6; 8; 10], odds = [1; 3; 5; 7; 9] *)

let words = ["apple"; "banana"; "fig"; "cherry"; "kiwi"]
let (short, long) = List.partition (fun w -> String.length w <= 4) words
(* short = ["fig"; "kiwi"], long = ["apple"; "banana"; "cherry"] *)
```

### fold (reduce)

```ocaml
let numbers = [1; 2; 3; 4; 5]

(* fold_left: left-to-right, tail-recursive (prefer this) *)
let total   = List.fold_left (+) 0 numbers        (* 15 *)
let product = List.fold_left ( * ) 1 numbers      (* 120 *)

(* fold_right: right-to-left, NOT tail-recursive *)
let total = List.fold_right (+) numbers 0          (* 15 *)

(* Build a reversed list *)
let rev_nums = List.fold_left (fun acc x -> x :: acc) [] numbers
(* [5; 4; 3; 2; 1] *)

(* Word frequency map *)
let words = ["a"; "b"; "a"; "c"; "b"; "a"]
let freq =
  List.fold_left
    (fun map w ->
      let count = match List.assoc_opt w map with Some n -> n | None -> 0 in
      (w, count + 1) :: List.filter (fun (k, _) -> k <> w) map)
    [] words
```

### flatten / concat_map (flatMap)

```ocaml
let nested = [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]

(* flatten (aka List.concat) *)
let flat = List.flatten nested
(* [1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(* concat_map = map then flatten (flatMap) *)
let sentences = ["hello world"; "foo bar"]
let words = List.concat_map (String.split_on_char ' ') sentences
(* ["hello"; "world"; "foo"; "bar"] *)

(* Cartesian product via concat_map *)
let colors = ["red"; "blue"]
let sizes  = ["S"; "M"; "L"]
let combos = List.concat_map (fun c -> List.map (fun s -> (c, s)) sizes) colors
(* [("red","S"); ("red","M"); ("red","L"); ("blue","S"); ...] *)
```

### sort / rev

```ocaml
let numbers = [3; 1; 4; 1; 5; 9; 2; 6]

(* sort is not in-place — returns a new list *)
let sorted = List.sort compare numbers
(* [1; 1; 2; 3; 4; 5; 6; 9] *)

(* Sort descending *)
let desc = List.sort (fun a b -> compare b a) numbers
(* [9; 6; 5; 4; 3; 2; 1; 1] *)

(* Sort by key *)
type person = { name: string; age: int }
let people = [{ name="Bob"; age=25 }; { name="Alice"; age=30 }]
let by_age = List.sort (fun a b -> compare a.age b.age) people

(* rev — O(n), returns a new list *)
let reversed = List.rev [1; 2; 3; 4; 5]
(* [5; 4; 3; 2; 1] *)

(* Seq has no rev — sequences may be infinite, so reversal is undefined.
   Convert to a list first: List.rev (List.of_seq seq) *)
```

### any / all

```ocaml
let numbers = [1; 2; 3; 4; 5]

let has_even = List.exists (fun x -> x mod 2 = 0) numbers  (* true *)
let all_pos  = List.for_all (fun x -> x > 0) numbers       (* true *)
let all_even = List.for_all (fun x -> x mod 2 = 0) numbers (* false *)
```

### zip / unzip

```ocaml
let names = ["Alice"; "Bob"; "Charlie"]
let ages  = [30; 25; 35]

(* zip: List.combine *)
let zipped = List.combine names ages
(* [("Alice", 30); ("Bob", 25); ("Charlie", 35)] *)

(* unzip: List.split *)
let (names2, ages2) = List.split zipped
(* names2 = ["Alice"; "Bob"; "Charlie"], ages2 = [30; 25; 35] *)
```

### take / drop

```ocaml
(* No built-in take/drop in stdlib; easy to write *)
let rec take n lst =
  if n <= 0 then []
  else match lst with
    | []     -> []
    | x :: rest -> x :: take (n - 1) rest

let rec drop n lst =
  if n <= 0 then lst
  else match lst with
    | []     -> []
    | _ :: rest -> drop (n - 1) rest

let first3 = take 3 [1; 2; 3; 4; 5]  (* [1; 2; 3] *)
let after2 = drop 2 [1; 2; 3; 4; 5]  (* [3; 4; 5] *)
```

### scan (running totals)

```ocaml
(* No built-in scan; implement with fold_left.
   Accumulate in reverse, then List.rev — O(n) rather than O(n²). *)
let scan_left f init lst =
  let (_, rev_acc) =
    List.fold_left
      (fun (prev, results) x ->
        let next = f prev x in
        (next, next :: results))
      (init, [init])
      lst
  in
  List.rev rev_acc

let running_sum = scan_left (+) 0 [1; 2; 3; 4; 5]
(* [0; 1; 3; 6; 10; 15] *)
```

### unique (dedup)

```ocaml
(* List.sort_uniq: sort and remove duplicates *)
let nums = [3; 1; 4; 1; 5; 9; 2; 6; 5; 3]
let unique = List.sort_uniq compare nums
(* [1; 2; 3; 4; 5; 6; 9] *)

(* Preserve order with fold_left *)
let dedupe lst =
  List.rev @@
  List.fold_left
    (fun acc x -> if List.mem x acc then acc else x :: acc)
    [] lst

let unique_ordered = dedupe [3; 1; 4; 1; 5; 9; 2; 6; 5; 3]
(* [3; 1; 4; 5; 9; 2; 6] *)
```

---

## Pattern Matching

Pattern matching is central to OCaml FP — it's exhaustive (the compiler warns on missing cases) and works on any data type.

```ocaml
(* Match on values *)
let describe n =
  match n with
  | 0 -> "zero"
  | 1 -> "one"
  | n when n < 0 -> "negative"
  | _ -> "large"

(* Match on lists *)
let rec sum = function
  | []      -> 0
  | x :: rest -> x + sum rest

(* Match on tuples *)
let fst_gt_snd = function
  | (a, b) when a > b -> true
  | _                  -> false

(* Match on algebraic data types *)
type shape =
  | Circle    of float
  | Rectangle of float * float
  | Triangle  of float * float * float

let area = function
  | Circle r          -> Float.pi *. r *. r
  | Rectangle (w, h)  -> w *. h
  | Triangle (a, b, c) ->
      let s = (a +. b +. c) /. 2.0 in
      sqrt (s *. (s -. a) *. (s -. b) *. (s -. c))

(* Nested pattern matching *)
let head_of_nonempty = function
  | x :: _ -> x
  | []     -> failwith "empty list"
```

---

## Option Type

`option` is OCaml's native way to represent nullable values. It has two constructors: `None` and `Some x`. Use it instead of `null` or exceptions for expected absence.

```ocaml
(* option type definition (built-in) *)
(* type 'a option = None | Some of 'a *)

let safe_divide a b =
  if b = 0 then None else Some (a / b)

let safe_sqrt x =
  if x < 0.0 then None else Some (sqrt x)

(* Pattern match to unwrap *)
let result = match safe_divide 10 2 with
  | None   -> "error"
  | Some n -> string_of_int n   (* "5" *)

(* Option.map: transform the value if present *)
let doubled = Option.map (fun x -> x * 2) (Some 5)  (* Some 10 *)
let nothing = Option.map (fun x -> x * 2) None       (* None *)

(* Option.bind: chain operations that return options (flatMap) *)
let process a b =
  safe_divide a b
  |> Option.bind (fun x -> safe_sqrt (Float.of_int x))

let () =
  match process 16 4 with
  | Some v -> Printf.printf "%.2f\n" v   (* 2.00 *)
  | None   -> Printf.printf "error\n"

(* Option.value: unwrap with a default *)
let n = Option.value (safe_divide 10 0) ~default:(-1)  (* -1 *)

(* Option.to_list / filter_map work well with lists of options *)
let results = List.filter_map (safe_divide 10) [2; 0; 5; 0; 1]
(* [5; 2; 10] *)
```

---

## Result Type

`result` is OCaml's type for operations that can fail with a meaningful error. It has two constructors: `Ok value` and `Error msg`. Use it instead of exceptions for expected, recoverable failures.

```ocaml
(* result type definition (built-in) *)
(* type ('a, 'e) result = Ok of 'a | Error of 'e *)

let safe_divide a b =
  if b = 0 then Error "division by zero" else Ok (a / b)

let safe_sqrt x =
  if x < 0.0 then Error "negative input" else Ok (sqrt x)

(* Pattern match to handle both cases *)
let () =
  match safe_divide 10 2 with
  | Ok n    -> Printf.printf "result: %d\n" n   (* result: 5 *)
  | Error e -> Printf.printf "error: %s\n" e

(* Result.map: transform the Ok value, pass Error through unchanged *)
let doubled = Result.map (fun x -> x * 2) (Ok 5)      (* Ok 10 *)
let failed  = Result.map (fun x -> x * 2) (Error "oops") (* Error "oops" *)

(* Result.bind: chain fallible operations (flatMap on Result) *)
let process a b =
  safe_divide a b
  |> Result.bind (fun x -> safe_sqrt (Float.of_int x))

let () =
  match process 16 4 with
  | Ok v    -> Printf.printf "%.2f\n" v    (* 2.00 *)
  | Error e -> Printf.printf "error: %s\n" e

let () =
  match process 16 0 with
  | Ok _    -> ()
  | Error e -> Printf.printf "error: %s\n" e  (* error: division by zero *)

(* Result.get_or / unwrap with default via pattern match *)
let value = match safe_divide 10 2 with Ok n -> n | Error _ -> -1
(* 5 *)

(* Collect a list of Results — keep only Ok values *)
let parse_int s =
  match int_of_string_opt s with
  | Some n -> Ok n
  | None   -> Error ("not a number: " ^ s)

let results = List.map parse_int ["1"; "two"; "3"; "four"; "5"]
let successes = List.filter_map Result.to_option results
(* [1; 3; 5] *)

(* Fail on first error (traverse) *)
let all_or_first_error lst =
  List.fold_left
    (fun acc x ->
      match acc, x with
      | Error e, _      -> Error e
      | _, Error e      -> Error e
      | Ok acc', Ok v   -> Ok (acc' @ [v]))
    (Ok []) lst

let () =
  match all_or_first_error results with
  | Ok ns   -> Printf.printf "all ok: %d values\n" (List.length ns)
  | Error e -> Printf.printf "failed: %s\n" e   (* failed: not a number: two *)
```

---

## Recursion & Tail Recursion

OCaml does not have loops by default — recursion is the FP-native alternative. The compiler optimizes tail-recursive functions into loops.

```ocaml
(* Simple recursion (NOT tail-recursive — stack grows with input) *)
let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

(* Tail-recursive version using accumulator pattern *)
let factorial n =
  let rec go acc n =
    if n <= 1 then acc
    else go (acc * n) (n - 1)
  in
  go 1 n

(* Tail-recursive fibonacci *)
let fibonacci n =
  let rec go a b = function
    | 0 -> a
    | n -> go b (a + b) (n - 1)
  in
  go 0 1 n

(* Tail-recursive list reverse *)
let rev lst =
  let rec go acc = function
    | []      -> acc
    | x :: rest -> go (x :: acc) rest
  in
  go [] lst

(* Note: List.fold_left is tail-recursive; List.fold_right is NOT *)
(* Prefer fold_left when accumulating over large lists *)

(* Mutual recursion with and keyword *)
let rec is_even = function
  | 0 -> true
  | n -> is_odd (n - 1)
and is_odd = function
  | 0 -> false
  | n -> is_even (n - 1)
```

---

## Sequences (Lazy Evaluation)

The `Seq` module provides lazy, potentially infinite sequences. Values are computed on demand.

`Seq.take` and `Seq.drop` require **OCaml ≥ 4.14**. On older versions, implement them manually or use the `seq` opam package.

```ocaml
(* Create sequences *)
let naturals = Seq.ints 0           (* 0, 1, 2, 3, ... infinite *)
let from5    = Seq.ints 5           (* 5, 6, 7, 8, ... *)

(* take / drop (OCaml >= 4.14) *)
let first10 = Seq.take 10 naturals |> List.of_seq
(* [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] *)

(* map / filter are lazy — no work done until consumed *)
let even_squares =
  Seq.ints 1
  |> Seq.filter (fun x -> x mod 2 = 0)
  |> Seq.map (fun x -> x * x)
  |> Seq.take 5
  |> List.of_seq
(* [4; 16; 36; 64; 100] *)

(* Infinite fibonacci sequence *)
let fibonacci_seq =
  let rec go a b () = Seq.Cons (a, go b (a + b))
  in go 0 1

let first10_fibs = fibonacci_seq |> Seq.take 10 |> List.of_seq
(* [0; 1; 1; 2; 3; 5; 8; 13; 21; 34] *)

(* Convert collections to/from Seq *)
let from_list   = List.to_seq [1; 2; 3]
let back_to_lst = List.of_seq from_list

(* Seq.fold_left consumes the sequence *)
let total = Seq.ints 1 |> Seq.take 5 |> Seq.fold_left (+) 0
(* 15 *)

(* Unfold: generate sequence from a state *)
let countdown =
  Seq.unfold (fun n -> if n < 0 then None else Some (n, n - 1)) 5
  |> List.of_seq
(* [5; 4; 3; 2; 1; 0] *)
```

---

## Practical Examples

### Example 1: Data Processing Pipeline

```ocaml
type employee = {
  name       : string;
  department : string;
  salary     : float;
  active     : bool;
}

let employees = [
  { name = "Alice";   department = "Engineering"; salary = 80000.0; active = true  };
  { name = "Bob";     department = "Sales";       salary = 60000.0; active = false };
  { name = "Charlie"; department = "Engineering"; salary = 90000.0; active = true  };
  { name = "Diana";   department = "Marketing";   salary = 55000.0; active = true  };
  { name = "Eve";     department = "Engineering"; salary = 75000.0; active = true  };
]

(* 1. Names of active Engineering employees, sorted *)
let eng_names =
  employees
  |> List.filter (fun e -> e.active && e.department = "Engineering")
  |> List.map (fun e -> e.name)
  |> List.sort String.compare
(* ["Alice"; "Charlie"; "Eve"] *)

(* 2. Average salary of active Engineering employees *)
let avg_eng_salary =
  let eng = List.filter (fun e -> e.active && e.department = "Engineering") employees in
  let total = List.fold_left (fun acc e -> acc +. e.salary) 0.0 eng in
  total /. Float.of_int (List.length eng)
(* 81666.67 *)

(* 3. Department headcount and total salary (active only) *)
let dept_summary =
  employees
  |> List.filter (fun e -> e.active)
  |> List.fold_left
       (fun groups e ->
         match List.assoc_opt e.department groups with
         | Some (count, total) ->
             (e.department, (count + 1, total +. e.salary))
             :: List.filter (fun (d, _) -> d <> e.department) groups
         | None ->
             (e.department, (1, e.salary)) :: groups)
       []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let () =
  List.iter
    (fun (dept, (count, total)) ->
      Printf.printf "%s: %d employees, $%.0f total\n" dept count total)
    dept_summary
(* Engineering: 3 employees, $245000 total *)
(* Marketing:   1 employees, $55000 total  *)
```

### Example 2: String Processing

```ocaml
let words = ["hello"; "world"; "functional"; "ocaml"]

(* Uppercase words longer than 5 chars *)
let result =
  words
  |> List.filter (fun w -> String.length w > 5)
  |> List.map String.uppercase_ascii
(* ["FUNCTIONAL"; "OCAML"] *)

(* Total character count *)
let total_chars = List.fold_left (fun acc w -> acc + String.length w) 0 words
(* 25 *)

(* Join with separator *)
let sentence = String.concat " " words
(* "hello world functional ocaml" *)

(* Split text into words and count frequencies.
   Prepend updated entries (O(n) per lookup on assoc list, fine for small vocab).
   For large inputs, use a Hashtbl instead. *)
let word_freq text =
  text
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
  |> List.fold_left
       (fun map w ->
         match List.assoc_opt w map with
         | Some n -> (w, n + 1) :: List.filter (fun (k, _) -> k <> w) map
         | None   -> (w, 1) :: map)
       []
  |> List.sort (fun (_, a) (_, b) -> compare b a)

let () =
  let freq = word_freq "the quick brown fox the fox the" in
  List.iter (fun (w, n) -> Printf.printf "%s: %d\n" w n) freq
(* the: 3, fox: 2, quick: 1, brown: 1 *)
```

### Example 3: Chaining Optional Operations

```ocaml
let safe_div a b =
  if b = 0 then None else Some (a / b)

let safe_isqrt n =
  if n < 0 then None else Some (int_of_float (sqrt (float_of_int n)))

(* Chain with Option.bind (like flatMap on Option) *)
let process a b =
  safe_div a b
  |> Option.bind safe_isqrt

let () =
  let show = function Some n -> string_of_int n | None -> "error" in
  Printf.printf "%s\n" (show (process 16 1));   (* 4 *)
  Printf.printf "%s\n" (show (process 16 0));   (* error *)
  Printf.printf "%s\n" (show (process (-16) 1)) (* error *)

(* Filter None values from a list *)
let safe_results = List.filter_map (safe_div 12) [3; 0; 4; 0; 6]
(* [4; 3; 2] *)
```

### Example 4: Grouping with Fold

```ocaml
(* Group list elements by a key function.
   Prepend to each bucket (O(n) overall), then reverse buckets at the end. *)
let group_by key lst =
  let groups =
    List.fold_left
      (fun groups x ->
        let k = key x in
        match List.assoc_opt k groups with
        | Some vs -> (k, x :: vs) :: List.filter (fun (g, _) -> g <> k) groups
        | None    -> (k, [x]) :: groups)
      [] lst
  in
  List.map (fun (k, vs) -> (k, List.rev vs)) groups

let words = ["apple"; "banana"; "apricot"; "blueberry"; "cherry"]

let by_first_letter = group_by (fun w -> w.[0]) words
(* [('c', ["cherry"]); ('b', ["banana"; "blueberry"]); ('a', ["apple"; "apricot"])] *)

let numbers = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
let even_odd = group_by (fun x -> if x mod 2 = 0 then "even" else "odd") numbers
(* [("even", [2;4;6;8;10]); ("odd", [1;3;5;7;9])] *)
```

### Example 5: Algebraic Data Types as Data Pipelines

```ocaml
(* Model a simple expression evaluator *)
type expr =
  | Num  of float
  | Add  of expr * expr
  | Mul  of expr * expr
  | Neg  of expr

let rec eval = function
  | Num n       -> n
  | Add (a, b)  -> eval a +. eval b
  | Mul (a, b)  -> eval a *. eval b
  | Neg e       -> -. (eval e)

let rec to_string = function
  | Num n       -> string_of_float n
  | Add (a, b)  -> Printf.sprintf "(%s + %s)" (to_string a) (to_string b)
  | Mul (a, b)  -> Printf.sprintf "(%s * %s)" (to_string a) (to_string b)
  | Neg e       -> Printf.sprintf "(-%s)" (to_string e)

(* (2 + 3) * -(4) *)
let expr = Mul (Add (Num 2.0, Num 3.0), Neg (Num 4.0))

let () =
  Printf.printf "%s = %g\n" (to_string expr) (eval expr)
  (* "(2. + 3.) * (-4.) = -20" *)
```

---

## Common Patterns

### Avoid Mutation — Use Functional Updates

```ocaml
(* Arrays have mutable cells — elements can be changed with arr.(i) <- v.
   Prefer lists (persistent, immutable) in FP-style code. *)

(* Bad: in-place mutation of an array *)
let bad_double arr =
  for i = 0 to Array.length arr - 1 do
    arr.(i) <- arr.(i) * 2  (* Mutates in place! *)
  done

(* Also fine but returns a new array — Array.map does NOT mutate *)
let double_array arr = Array.map (fun x -> x * 2) arr

(* Good: lists are persistent data structures *)
let double_list lst = List.map (fun x -> x * 2) lst

(* Functional record update *)
type config = { debug: bool; timeout: int; host: string }
let default_config = { debug = false; timeout = 30; host = "localhost" }

(* Create modified copy without touching the original *)
let dev_config   = { default_config with debug = true }
let custom_config = { default_config with timeout = 60; host = "prod.example.com" }
```

### Prefer `fold_left` over Explicit Recursion

```ocaml
(* Explicit recursion (verbose) *)
let rec sum = function
  | []      -> 0
  | x :: rest -> x + sum rest

(* fold_left (concise, tail-recursive) *)
let sum lst = List.fold_left (+) 0 lst

(* Compute multiple things in one pass *)
let stats lst =
  let (n, total, mn, mx) =
    List.fold_left
      (fun (count, sum, mn, mx) x ->
        (count + 1, sum + x, min mn x, max mx x))
      (0, 0, max_int, min_int)
      lst
  in
  (n, total, mn, mx)

let (count, total, minimum, maximum) = stats [3; 1; 4; 1; 5; 9; 2; 6]
```

### Use `@@` to Avoid Parentheses

```ocaml
(* With parentheses *)
let result = string_of_int (List.length (List.filter (fun x -> x > 3) [1;2;3;4;5]))

(* With @@ (right-associative application) *)
let result = string_of_int @@ List.length @@ List.filter (fun x -> x > 3) [1;2;3;4;5]

(* With |> (usually most readable for pipelines) *)
let result =
  [1;2;3;4;5]
  |> List.filter (fun x -> x > 3)
  |> List.length
  |> string_of_int
```

---

## Quick Reference

### List module (most-used)

| Function | Signature | Description |
|:---------|:----------|:------------|
| `List.map` | `('a -> 'b) -> 'a list -> 'b list` | Transform each element |
| `List.filter` | `('a -> bool) -> 'a list -> 'a list` | Keep matching elements |
| `List.filter_map` | `('a -> 'b option) -> 'a list -> 'b list` | Filter and transform in one pass |
| `List.fold_left` | `('acc -> 'a -> 'acc) -> 'acc -> 'a list -> 'acc` | Reduce left-to-right (tail-recursive) |
| `List.fold_right` | `('a -> 'acc -> 'acc) -> 'a list -> 'acc -> 'acc` | Reduce right-to-left (not tail-recursive) |
| `List.concat_map` | `('a -> 'b list) -> 'a list -> 'b list` | Map then flatten (flatMap) |
| `List.flatten` | `'a list list -> 'a list` | Collapse one level of nesting |
| `List.partition` | `('a -> bool) -> 'a list -> 'a list * 'a list` | Split into two lists |
| `List.exists` | `('a -> bool) -> 'a list -> bool` | Any element matches? |
| `List.for_all` | `('a -> bool) -> 'a list -> bool` | All elements match? |
| `List.find` | `('a -> bool) -> 'a list -> 'a` | First matching element (raises Not_found) |
| `List.find_opt` | `('a -> bool) -> 'a list -> 'a option` | First matching element, safe |
| `List.sort` | `('a -> 'a -> int) -> 'a list -> 'a list` | Sort (not in place) |
| `List.sort_uniq` | `('a -> 'a -> int) -> 'a list -> 'a list` | Sort and deduplicate |
| `List.rev` | `'a list -> 'a list` | Reverse |
| `List.length` | `'a list -> int` | Count elements |
| `List.nth` | `'a list -> int -> 'a` | Element at index (raises if out of bounds) |
| `List.combine` | `'a list -> 'b list -> ('a * 'b) list` | Zip two lists |
| `List.split` | `('a * 'b) list -> 'a list * 'b list` | Unzip |
| `List.assoc_opt` | `'a -> ('a * 'b) list -> 'b option` | Lookup in association list |
| `List.map2` | `('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list` | Map over two lists |
| `List.iter` | `('a -> unit) -> 'a list -> unit` | Iterate for side effects |
| `List.append` / `@` | `'a list -> 'a list -> 'a list` | Concatenate |

### Option module

| Function | Description |
|:---------|:------------|
| `Option.map f opt` | Apply `f` to the value if `Some`, pass `None` through |
| `Option.bind opt f` | Chain a function returning `option` (flatMap) |
| `Option.value opt ~default` | Unwrap with a fallback |
| `Option.is_some` / `Option.is_none` | Test presence |
| `Option.to_list` | `Some x` → `[x]`, `None` → `[]` |

### Result module

| Function | Description |
|:---------|:------------|
| `Result.map f r` | Apply `f` to `Ok` value, pass `Error` through |
| `Result.bind r f` | Chain a function returning `result` (flatMap) |
| `Result.is_ok` / `Result.is_error` | Test outcome |
| `Result.to_option` | `Ok x` → `Some x`, `Error _` → `None` |

### Seq module (lazy sequences, OCaml ≥ 4.07)

| Function | Description |
|:---------|:------------|
| `Seq.ints n` | Infinite sequence `n, n+1, n+2, ...` |
| `Seq.map f s` | Lazy map |
| `Seq.filter pred s` | Lazy filter |
| `Seq.take n s` | First n elements (≥ 4.14) |
| `Seq.drop n s` | Skip n elements (≥ 4.14) |
| `Seq.fold_left f init s` | Consume and reduce |
| `Seq.unfold f init` | Generate sequence from state |
| `List.to_seq` / `List.of_seq` | Convert between list and sequence |

### Fun module (OCaml ≥ 4.08)

| Function | Description |
|:---------|:------------|
| `Fun.id` | Identity: `fun x -> x` |
| `Fun.const x` | Always returns `x`, ignores second argument |
| `Fun.flip f` | Swap first two arguments of `f` |
| `Fun.compose f g` | `f ∘ g`: apply `g` then `f` (≥ 4.13) |

---

## Key Takeaways

1. **Immutable by default**: No need to reach for `ref` — build new values instead of mutating
2. **Curried by default**: Every function is automatically curried; partial application is free
3. **`|>` is your friend**: Pipe operator turns nested calls into readable left-to-right pipelines
4. **Pattern match everything**: Exhaustive pattern matching catches missing cases at compile time
5. **`Option` over `null`**: Use `Some`/`None` with `Option.map` and `Option.bind` to chain nullable operations safely
6. **`Result` over exceptions**: Use `Ok`/`Error` with `Result.bind` for recoverable failures
7. **Tail recursion matters**: Use accumulator pattern or `fold_left` for large inputs to avoid stack overflow
8. **`Seq` for laziness**: Use `Seq` when you need lazy or infinite sequences — it computes on demand
9. **ADTs + pattern matching = expressive data modeling**: Algebraic data types let you model domains precisely and handle all cases
