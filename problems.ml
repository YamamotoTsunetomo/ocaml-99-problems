(* 99 problems *)




(* Working with lists *)

(* 1. Write a function last : 'a list -> 'a option that 
returns the last element of a list. (easy) *)
let rec last = function
    | [] -> None
    | [x] -> Some x
    | h::t -> last t

(* 2. Find the last but one 
(last and penultimate) elements of a list. (easy) *)
let rec last_two = function
    | [] | [_] -> None
    | [a;b] -> Some (a, b)
    | _::t -> last_two t

(* 3. Find the K'th element of a list. (easy) *)
let rec at k = function
    | [] -> None
    | h::t -> if k = 0 then Some h else at (k - 1) t

(* 4. Find the number of elements of a list. (easy) *)
let length lst = 
    let rec aux acc = function
        | [] -> acc
        | _::t -> aux (acc + 1) t in
        aux 0 lst

(* 5. Reverse a list. (easy) *)
let rev lst = 
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (h :: acc) t in
        aux [] lst

(* 6. Find out whether a list is a palindrome. (easy) *)
let is_palindrome lst = lst = rev lst


(* 7. Flatten a nested list structure. (medium) *)
type 'a node = 
    | One of 'a
    | Many of 'a node list

let flatten node = 
    let rec aux acc = function
        | [] -> acc
        | One x :: t -> aux (acc @ [x]) t  
        | Many x :: t -> aux (aux acc x) t in
    aux [] node

(* 8. Eliminate consecutive duplicates
 of list elements. (medium) *)
let compress = 
    let rec aux acc curr = function
        | [] -> acc
        | h::t -> if h=curr then aux acc curr t 
            else aux (acc @ [h]) h t in
    function 
        | [] -> []
        | h::t -> aux [h] h t

(* 9. Pack consecutive duplicates
 of list elements into sublists. (medium) *)
let pack lst = 
    let rec aux curr acc = function
        | [] -> []
        | [x] -> (x :: curr) :: acc
        | a :: (b :: _ as t) -> 
            if a = b then aux (a :: curr) acc t
            else aux [] ((a :: curr) :: acc) t in
    rev (aux [] [] lst)

(* 10. Run-length encoding of a list. (easy) *)
let encode lst = 
    let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (acc @ [(length h, List.hd h)]) t in
    aux [] (pack lst)

(* 11. Modified run-length encoding. (easy) *)
type 'a rle =
    | One of 'a
    | Many of int * 'a

let modified_encode lst = 
    let rec aux acc count = function 
    | [] -> acc
    | [x] -> if count = 0 then acc @ [One (x)]
    else acc @ [Many (count + 1, x)]
    | a::(b::_ as t) -> if a = b then aux acc (count + 1) t
    else if count = 0 then aux (acc @ [One a]) 0 t 
    else aux (acc @ [Many (count + 1, a)]) 0 t in 
    aux [] 0 lst 

(* 12. Decode a run-length encoded list. (medium) *)
let decode lst = 
    let rec aux acc = function
    | [] -> acc
    | x::(_ as t) -> match x with 
        | One (y) -> aux (acc @ [y]) t
        | Many (c,y) -> if c = 1 then 
        aux (acc @ [y]) t else aux (acc @ [y]) (Many(c-1, y)::t)
    in aux [] lst 

(* 13. Run-length encoding of a list (direct solution). (medium) *)
let direct_encode = modified_encode (* already done in (11) *)

(* 14. Duplicate the elements of a list. (easy) *)
let duplicate lst = 
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (acc @ [h] @ [h]) t in
    aux [] lst

(* 15. Replicate the elements of a list a given number of times. (medium) *)
let replicate lst n = 
    let rec repl_element a = function
        | 0 -> []
        | n -> a::repl_element a (n-1) in 
    let rec aux acc n = function 
        | [] -> acc 
        | h :: t -> aux (acc @ repl_element h n) n t in 
    aux [] n lst 

(* 16. Drop every N'th element from a list. (medium) *)
let drop lst n =
    let rec aux acc t = function 
        | [] -> acc 
        | h::hs-> if t = 1 then aux acc n hs
        else aux (acc @ [h]) (t-1) hs in
    aux [] n lst  

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
let split lst n = 
    let rec aux acc n = function 
        | [] -> (acc, [])
        | (h::t) as x -> if n = 0 then (acc, x)
        else aux (acc @ [h]) (n-1) t in 
    aux [] n lst 

(* 18. Extract a slice from a list. (medium) *)
let slice lst i k = 
    let rec aux acc x = function 
        | [] -> acc 
        | h::t -> if x > k then acc
        else if x < i then aux acc (x + 1) t
        else aux (acc @ [h]) (x + 1) t in 
    aux [] 0 lst 

(* 19. Rotate a list N places to the left. (medium) *)
let rotate lst n = 
    let rec aux acc lst n = if n < 0 then aux acc lst ((length lst)+n)
        else match lst with 
            | [] -> acc
            | (h::t) as x -> if n = 0 then x @ acc
                else aux (acc @ [h]) t (n - 1) in 
    aux [] lst n 

(* 20. Remove the K'th element from a list. (easy) *)
let remove_at k lst = 
    let rec aux acc k = function 
        | [] -> acc
        | h::t -> if k = 0 then acc @ t
        else aux (acc @ [h]) (k-1) t in 
    aux [] k lst 

(* 21. Insert an element at a given position into a list. (easy) *)
let insert_at el k lst = 
    let rec aux acc el k = function 
        | [] -> acc @ [el]
        | (h::t) as x -> if k = 0 then acc @ [el] @ x 
        else aux (acc @ [h]) el (k - 1) t in 
    aux [] el k lst 

(* 22. Create a list containing all integers within a given range. (easy) *)
let rec range a b = if a > b then rev (range b a) 
    else 
        let rec aux acc b = function
            | x -> if x = b then acc @ [b] else 
                aux (acc @ [x]) b (x + 1) in
    aux [] b a 

(* 23. Extract a given number of randomly selected
 elements from a list. (medium) *)
let rand_select lst n = 
    let len = length lst in 
    let rec aux acc = function 
        | 0 -> acc
        | x -> aux (acc @ [at (Random.int len) lst]) (x-1) in 
    aux [] n

(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
let lotto_select n m = 
    rand_select (range 1 m) n

(* 25. Generate a random permutation of the elements of a list. (easy) *)
let permutation lst =
    rand_select lst (length lst)

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
let rec extract k lst = 
    if k <= 0 then [[]]
    else match lst with
        | [] -> []
        | h::t -> (List.map(fun x -> h::x) (extract (k-1) t)) (@ extract k t)
