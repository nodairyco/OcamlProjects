module Ocaml_basics = struct (*this type of declaration isn't necessary for an ocaml file. 
   to see what it does jump to line 404*)
  (*expression*)
  let expr_example_1 = 3 + 4;;
  let expr_examp = 3 +
    4;;

  (*aux method*)
  let print_f b = Format.printf "%b@ \n" b;;

  (*operation examples*)
  let operation_example_1 = -3.0/.4.0;;
  let operation_example_2 = "So"^" "^"it"^" "^"goes";;
  let operation_example_3 = 1 > 2 || not (2.0<1.0);;

  (*``let`` keyword creates immutable variables*)
  let seven = 42;;
  let string_seven:string = string_of_int seven;;
  print_endline string_seven;;

  (*assigning new values to already created variables doesn't change the it's value, but creates
    a new variable with the same name.*)
  let seven = "seven";;
  print_endline seven;;

  (*pairs and tuples are objects which store 2 and 2 or more values, respectively. 
    the vaues don't have to have the same type*)
  let pair_example_1 = (3,4);;
  let pair_example_2 = (1=2,"hello");;
  let tuple_example_1 = (2,3,4,5);;
  let tuple_example_2 = ("hello",true,3.14159);;

  (*2 or more variables may be instantiated at once*)
  let (x,y) = (3,4.0);;


  (*about Records*)
  (*Records are tuples with named components whose ordering therefore, is irrelevant. 
    They must be introduced with ``type`` keyword before being declared with ``let``. 
    Type names and record components must begin with small letters*)
  type record_example = {age:int; name:string};;
  let record_example_object = { age=77; name="Fripp" };;
  let record_example_object_2 = {age=74; name="Bruford"};;
  let record_example_object_3 = {age=74; name="Bruford"};;

  (*just a function that prints a boolean*)
  let bool_record = record_example_object_2=record_example_object_3;;
  print_f bool_record;;

  (*individual components can be accessed like attributes in java*)
  record_example_object_3.age;;

  (*we can make a new record with pattern matching*)
  let {age=x; name=y} = record_example_object_2;;

  (*same can be done with just any number of components*)
  let {age=x; _} = record_example_object_3;;

  (*Case distinction: match and if*)
  (*basically the same thing as java's switch case. Used a lot here, learn it well*)
  let matching_example_func_0 n = match n 
    with 0 -> "null"
    | 1 -> "one"
    | _ -> "uncountable";;

  let matching_example_string : string = let n = 4 
    in matching_example_func_0 n;;

  print_endline matching_example_string;;
  (*pattern matching must always be exhaustive, or else warnings and exceptions may arise*)

  (*lists*)
  (*ocaml lists resemble those of python with declaration, and manipulation. 
    all elements in a list must be of the same type*)
  (*some ways to make a list*)
  let mt = [];; (*creates an empty list*)
  let l1 = 1::mt;; (*concatinates 1 with mt, and stores the newly made list in l1*)
  let l = [1;2;3];; (*"normal" way of creating lists*)
  let l1 = 1::2::3::[];; (*same thing as the "normal" declaration, but achieved through concatinating list's components with an empty list*)

  (*pattern matching with lists*)
  match l 
    with [] -> 1 
    | x::xs -> x;;(*not quite sure what x::xs does, but I think it is probably a for each loop*)

  (*functions*)
  (*functions in ocaml resemble mathematical functions. They are treated the same as variables. The name of a function is the same as a variable, whose value is a function*)
  let double x = 2*x;;
  let double = fun x -> x*2;; (*these two declarations do the same thing*)

  (double 3, double (double 1));; (*creates a pair*)

  (*declaring recursive functions*)
  let rec fac n = if n<2 then 1 else n * fac (n-1);;
  let rec fib = fun x -> if x <= 1 then 1 else fib(x-1) + fib(x-2);;

  (*if a function calls itself via another function it is called mutually recursive*)
  (*meanings of 2 functions can be conbined into 1 with ``and``*)
  let rec even = fun n -> if n=0 then "even" else odd(n-1)
    and odd = fun n -> if n=0 then "odd" else even(n-1);;

  (*even case distinction can be recursive*)
  let rec len = fun l -> match l 
    with [] -> 0 
    | x::xs -> 1 + len xs;;

  (*this can be shortened to:*)
  let rec len = function [] -> 0 
    | x::xs -> 1 + len(xs);;

  (*recursive case distinction with multiple arguments*)
  (*the following function just concatinates 2 lists*)
  let rec concat l y = match l 
    with [] -> y
    | x::xs -> x::concat xs y;;

  (*can also be written as*)
  let rec concat = function [] -> fun y -> y
    | x::xs -> fun y -> x::concat xs y;;

  (*it is possible to create functions within functions with ``let``*)
  let x = 5 
    in let sq = x*x 
    in sq+sq;;

  let facit n = 
    let rec iter m yet = if m>n then yet 
      else iter (m+1) (m*yet) 
    in iter 1 2;;

  (*enumerations (enums) in ocaml*)
  type colour = Diamonds | Hearts | Gras | Clubs;;
  type value = Seven | Eight | Nine | Ten | Jack | Queen | King | Ace;;
  (*each name seperated by ``|`` is called a constructor, and each one of them must begin with a capital 
      letters*)

  let diamonds_ace = (Diamonds,Ace);;

  (*since these are enums, each constructor recieves a value ranging from lowest to highets based on their 
     creation*)
  let bool_constructor_1 = Diamonds>Hearts;;
  print_f bool_constructor_1;;
  let bool_constructor_2 = Hearts>Diamonds;;
  print_f bool_constructor_2;;

(*pattern matching on constructors*)
  let is_trump = function 
    | (Hearts, _) -> true
    | (_, Jack) -> true
    | (_, Queen) -> true
    | (_, _) -> false;;

  print_f (is_trump (Hearts, Ten));;
  print_f (is_trump (Gras, Eight));;

  let get_colour c1 = match c1 
    with (Diamonds, _) -> Diamonds 
    | (Hearts, _) -> Hearts
    | (Gras, _) -> Gras
    | (Clubs, _) -> Clubs;;

  let string_of_colours colour = match colour
    with Diamonds -> "Diamonds"
    | Hearts -> "Hearts"
    | Gras -> "Gras"
    | Clubs -> "Clubs";;
  
  print_string (string_of_colours (get_colour diamonds_ace));; (*idk why this doesnt print*)
  print_endline " ";;
  (*sum modules aka optionals are enums whose' constructors take arguments and the types can now have 
     distinct methods*)
  type 'a option = None | Some of 'a;;
  let is_some x = match x 
    with Some _ -> true 
    | None -> false;;

  let value x a = match x with 
    | Some y -> y 
    | None -> a;;

  let map f x = match x with 
    | Some y -> Some (f y)
    | None -> None;;

  let join a = match a with 
    | Some a' -> a'
    | None -> None;;

  (*option types ('a, 'b, 'c ...) are like generics in java, as they dont have a specific 
      value assigned to them*)
  let rec get_value a l = match l 
    with [] -> None
    | (b,z)::rest -> if a=b then Some z else get_value a rest;;

  (*just like java lists, types can be recursive too*)
  (*same thing, but like generic lists*)
  type 'a sequence = End | Next of ( 'a * 'a sequence);;
  Next ("Somethig", Next ("Something", End));;

  let rec nth n s = match (n,s) with 
    | (_, End) -> None
    | (0, Next (x, _)) -> Some x 
    | (n, Next (_, rest)) -> nth (n-1) rest;;

  let rec down = function 
    0 -> End
    | n -> Next (n, down (n-1));;

  (*tail recursive functions*)
  (*a function is tail-recursive if only the final call of the function is recursive, and furthermore, 
      only computes the recursion and nothing else*)
  (*tail recursive function*)
  let rec fac1 = function
    (1,acc) -> acc
    | (n,acc) -> fac1 (n-1,n*acc)
  ;;

  (*non tail recursive function*)
  let rec loop x = 
    if x<2 
      then x
    else if x mod 2 = 0 
      then loop (x/2)
    else 
      loop (3*x+1)
  ;;

  (*tail recursion is useful when working with lists*)
  (*a function that reverses a list, and is tail recursive*)
  let rec rev list = match list
    with [] -> []
    | x::xs -> concat (rev xs) [x]
  ;;

  (*higher order functions and currying*)
  (*they are the same thing. it's when a function returns another function*)
  let normal_func (a,b) = a+b;;
  let currying_func f a b = f (a,b);;
  let plus2 = currying_func normal_func 2;;
  let plus3 = currying_func normal_func 3;;
  let currying_result_int = plus2 (plus3 4);;
  print_endline (string_of_int currying_result_int);;

  (*some list functions*)
  (*all of these functions are ``list functionals`` aka, they dont care about what kind of list the input
    is. it only controls recursion, the incoming functions define list types*)

  (*mapping a list to another list*)
  (*takes two arguments, a function f, and a list list, and applys f to every element of the list*)
  let rec map f = function 
    [] -> []
    | x::xs -> f x :: map f xs;;

  let mapping_example_list = [1;2;3;4];;
  let mapping_example_function f = string_of_int f;;
  let mapping_example_result = map mapping_example_function mapping_example_list;; 
  (*this also demonstrates curring as ``mapping_example_functions returns a functions, which turns an int
    into string*)

  (*a function that concatinates the incoming list from the left, to the present one, and maps it with
    incoming function f*)
  let rec fold_left f a = function 
    [] -> a
    | x::xs -> fold_left f (f a x) xs;;

  (*same but from the right*)
  let rec fold_right f = function 
    [] -> fun b -> b
    | x::xs -> fun b -> f x (fold_right f xs b);;

  (*searches in a list and finds the value youre looking for, and returns an optional of it, else returns
    an empty optional*)
  let rec find_opt f = function
    [] -> None
    | x::xs -> 
    if f x
      then  
        Some x
    else 
      find_opt f xs;;

  (*polymorphic data-types*)
  (*much like in java how we can create generic datatypes with <T> in ocaml we can make generic datatypes with
    ``'a``*)
  type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree);;

  (* ``'a`` in this case can be any type that we want. Leaf stores only 1 value of, and Node stores pairs
    other tree nodes, much like Trees in java (see Penguin Trees for reference)*)
  let leaf_example = Leaf 1;;
  let node_example = Node (leaf_example, (Leaf 2));;
  (*this creates a tree of int*)
  (*we can also make a tree of other data types*)
  (*pair tree*)
  let pair_tree_node = Node (Node (Leaf (1, "2"), Leaf (2, "3")), Leaf (5, "8"));;

  (*list tree*)
  let list_tree_node = Node (Leaf [1;2;3], Node (Leaf [2;7], Leaf [6;]));;

  (*since tree is a polymorphic datatype its functions also will be polymorphic*)
  let rec size = function 
      Leaf _ -> 1
      | Node(t, t') -> size t + size t'
  ;;
  (*t, and t' in this case are like generics in java <T extends TreeNode>*)
  let rec flatten = function
      Leaf x -> [x]
      |Node(t, t') -> flatten t @ flatten t'
  ;;

  let flatten1 f = 
    let rec aux = function
      (Leaf x, xs) -> x :: xs
      |(Node(t,t'), xs) -> let xs = aux (t', xs) in 
      aux(t, xs)
  in
    aux (f, [])
  ;;

  (*the queue data-structure*)
  (*in java it was represented with an array, and controlled via pointers. here it is represented with 2
    generic lists*)
  type 'a queue = Queue of 'a list * 'a list;;

  let is_queue_empty = function 
      Queue([],[])-> true
      | _ -> false
  ;;

  let queue_to_list = function
      |Queue (first, []) -> first
      |Queue (first, last) -> first @ List.rev last
  ;;

  let enqueue x (Queue(first,last)) = Queue(first, x::last)
  ;;
  let dequeue = function 
    |Queue([],last)-> (
      match List.rev last with 
        | [] -> (None, Queue([],[]))
        | x::xs -> (Some x, Queue(xs, []))
        )
    |Queue(x::xs, last) -> (Some x, Queue(xs,last))
  ;;

  (*ocaml supports lambda expressions through anonymous functions, functions without names*)
  (*lets create a mapping with an anonymous function*)
  let anon_map_list = map (fun x -> x*x) [1;2;3];;
  (*more often than not, they are used when a function is supposed to return a funciton*)

  (*handling exceptions*)
  (*this can be done with pattern matching with try*)
  let divide (n,m) = try Some (n/m)
    with Division_by_zero -> None
  ;;
  (*divides the parameters with eachother and returns an optional of them. if a Division_by_zero error is
    thrown returns an empty optional*)

  let rec member l x = try if x = List.hd l then true 
      else member (List.tl l) x 
    with Failure _ -> false
  ;; 
  (*checkes if x is in l with List.hd. This method throws an exception if l is empty, we catch it and 
    return false if this happens*)

  (*we can throw our on exceptions with ``raise``*)
  exception Hell of string;;
  let rec dont_let_list_contain_4 = function
    | [] -> []
    | x::xs -> 
      if x = 4 
        then 
          raise (Hell "list containes 4!") 
      else dont_let_list_contain_4 xs 
  ;;
  dont_let_list_contain_4 [1;2;3;4];;

  (*modules*)
  (*much like structs in c*)
  (*fields defined inside modules are like fields for java classes. they can be accessed statically
    and dinamically, but static access only returns functions, this means we can have several 
    implementations of functions*)
  module Pairs = 
    struct
      type 'a pair = 'a * 'a
      let pair (a, b) = (a,b)
      let first (a, b) = a
      let second (a, b) = b
  end
  ;;

  module Triplets = struct 
    type 'a triplet = Triplet of 'a * 'a * 'a
    let first (Triplet (a,_,_)) = a
    let second (Triplet (_,b,_)) = b
    let third (Triplet (_,_,c)) = c
  end;; 

  module Pair2 = struct
    type 'a pair = 'a * 'a 
    let first ab = ab true 
    let second ab = ab false
  end;;

  (*to avoid writing same code over and over again we can make every aspect of a module available with
    the ``open`` keyword*)
  module A = struct
    let x = 1
  end;;

  module B = struct
    open A 
    let y = 2
  end
  ;;
  (*``include`` keyword allows us to copy definitions of structs into another struct*)
  module C = struct
    include A
    include B
  end;;

  let a_b = C.x + C.y;;

  (*we can have nested Modules in modules*)
  module Quads = struct
    module Pairs = struct
      type 'a pair = 'a * 'a 
      let pair (a,b) = (a,b)
      let first (a,_) = a
      let second (_,b) = b
    end;;
    type 'a quad = 'a Pairs.pair Pairs.pair;;
    let quad (a,b,c,d) = Pairs.pair (Pairs.pair (a,b), Pairs.pair (c,d));;
    let first q = Pairs.first(Pairs.first q);;
    let second q = Pairs.first(Pairs.second q);;
    let third q = Pairs.second(Pairs.first q);;
    let fourth q = Pairs.second(Pairs.second q);;
  end;;
  let quads_example = 
    let temp_quad = Quads.quad (1,2,3,4) 
  in
    Quads.Pairs.first temp_quad;;

  (*if we want to restrict access to fields or functions in modules we do this with the ``sig`` keyword*)
  module Sort = struct
    let single l = map (fun x -> [x]) l
    let rec merge l1 l2 = match (l1,l2) with
    |([], _) -> l2
    |(_, []) -> l1
    |(x::xs, y::ys) -> if x<y then x::merge xs l2 else y::merge l1 ys
    let rec merge_lists = function 
    [] -> []
    |[l] -> [l]
    |l1::l2::ll -> merge l1 l2 :: merge_lists ll
    let sort list = let list = single list 
    in let rec doit = function 
    [] -> []
    |[l] -> l
    |l -> doit (merge_lists l)
    in
    doit list 
  end;;
  (*this implementations allows access to auxiliary methods ``single`` and ``merge_list``*)
  (*the following hides those functions*)
  module type Sort = sig
    val merge : 'a list -> 'a list -> 'a list
    val sort : 'a list -> 'a list 
  end;;
  
  module Sort2 : Sort = struct
    let single l = map (fun x -> [x]) l

    let rec merge l1 l2 = match (l1,l2) with
    |([], _) -> l2
    |(_, []) -> l1
    |(x::xs, y::ys) -> if x<y then x::merge xs l2 else y::merge l1 ys

    let rec merge_lists = function 
    [] -> []
    |[l] -> [l]
    |l1::l2::ll -> merge l1 l2 :: merge_lists ll

    let sort list = let list = single list 
    in let rec doit = function 
    [] -> []
    |[l] -> l
    |l -> doit (merge_lists l)
    in
    doit list 

  end;;
  
  (*with this kind of implementation we cannot access functions which aren't defined in Sort module type*)

  (*functors*)
  (*functors are higherorder modules (modules which take modules as inputs)*)
  (*to create a functor first we specify signatures for it's arguments, and its own signature*)
  module type Decons = 
  sig
    type 'a t
    val decons : 'a t -> ('a * 'a t) option
  end;;

  module type GenFold = functor (X:Decons) -> sig
      val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a X.t -> 'b 
      val fold_right : ('a -> 'b -> 'b) -> 'a X.t -> 'b -> 'b 
      val size : 'a X.t -> int 
      val list_of : 'a X.t -> 'a list
      val iter : ('a -> unit) -> 'a X.t -> unit
    end;;

  module Fold : GenFold = functor (X:Decons) -> struct
    let rec fold_left f b t = match X.decons t with 
    |None -> b
    |Some (x,t) -> fold_left f (f b x) t

    let rec fold_right f t b = match X.decons t with 
    |None -> b
    |Some (x,t) -> f x (fold_right f t b)

    let size t = fold_left (fun a x -> a+1) 0 t

    let list_of t = fold_right (fun x xs -> x::xs) t []

    let iter f t = fold_left (fun () x -> f x) () t
  end;;
  
  module MyQueue = struct 
    open Queue
    type 'a t = 'a queue
    let decons = function
    Queue([], xs) -> (match rev xs with
      |[] -> None
      |x::xs -> Some(x, Queue(xs, [])))
    |Queue(x::xs, t) -> Some(x, Queue(xs, t)) 
  end;;

  module FoldQueue = Fold(MyQueue);;
  (*Module FoldQueue which is made by passing MyQueue (a module which satisfies all requiremements of Decon
     signature) to the functor Fold.*)
   
end;;
(*end of ocaml basics, after this begins proofs, and parallel programming. know that this doesn't cover 
   much of folien, read it for yourself for better understanding*)