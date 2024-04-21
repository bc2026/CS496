(*
Fruit Basket Processors
*)

type 'a result = Ok of 'a | Error of string
type fruit = A | O | K
type 'a basket = 'a list

(* Sample fruit baskets *)
let fb1 : fruit basket = [A;A;O;A;K;K]
let fb2 : fruit basket = [A;A;A;A]

(** 
A fruit basket processor is any expression whose type is an
instance of:

[fruit basket -> 'a result]

Some examples of types that have this form are: 
Eg. [fruit basket -> int result]
Eg. [fruit basket -> bool result]
Eg. [fruit basket -> (fruit basket) result]

A fruit basket processor analyzes a fruit basket and can:
1. Either, fail (returning: Error s, with s a string)
2. Or, succeed (returning: Ok v, wwith v the result)
*)

(* 
Implement the following fruit basket processors.
NOTE: You are free to add the "rec" keyword just after the "let", if needed.
*)

(** [no_of_apples fb] fruit basket processor that returns the number of apples in the fruit basket [fb].
Eg. no_of_apples fb1 => Ok 3
*)                   
let no_of_apples : fruit basket -> int result =
fun fb ->
Ok (List.length (List.filter ((=)A) fb))

(** [no_of_oranges fb] fruit basket processor that returns the number of oranges in the fruit basket [fb].
Eg. no_of_oranges fb1 => Ok 3
*)                   
let no_of_oranges : fruit basket -> int result =
fun fb ->
Ok (List.length (List.filter ((=)O) fb))

(** [no_of_kiwis fb] fruit basket processor that returns the number of kiwis in the fruit basket [fb].
Eg. no_of_kiwis fb1 => Ok 3
*)                   
let no_of_kiwis : fruit basket -> int result =
fun fb ->
Ok (List.length (List.filter ((=)K) fb))

(** [remove_orange fb] fruit basket processor that removes one orange (the first one) from [fb]. If there are no oranges, it should return an error.
Eg. [remove_orange fb1] => [Ok [A;A;A;K;K]] 
Eg. [remove_orange fb2] => [Error "no oranges"]
*)

let rec rem_first_orange fb =
match fb with
| [] -> assert false
| O::t -> t
| h::t ->
h :: rem_first_orange t

let remove_orange : fruit basket -> (fruit basket) result =
fun fb ->
if List.mem O fb
then Ok (rem_first_orange fb)
else Error "no oranges"


(** [apples_to_oranges_ratio fb] fruit basket processor that returns
the quotient between the number of apples and the number of
oranges in [fb].
It should return [Error] if there are no oranges. 
Eg. apples_to_oranges_ratio fb1 => Ok 3
apples_to_oranges_ratio fb2 => Error
*)
let apples_to_oranges_ratio : fruit basket -> int result =
fun fb ->
let na = List.length (List.filter ((=)A) fb)
and no = List.length (List.filter ((=)O) fb)
in if no=0
then Error "no oranges"
else Ok (na/no)


(** [apples_to_kiwis_ratio fb] fruit basket processor that returns
the quotient between the number of apples and the number of
kiwis in [fb].
It should return [Error] if there are no kiwis. 
Eg. apples_to_kiwis_ratio fb1 => Ok 1
apples_to_kiwis_ratio fb2 => Error
*)    
let apples_to_kiwis_ratio : fruit basket -> int result =
fun fb ->
let na = List.length (List.filter ((=)A) fb)
and nk = List.length (List.filter ((=)K) fb)
in if nk=0
then Error "no kiwis"
else Ok (na/nk)


(** [ratio_sum fb] fruit basket processor that returns the sum of the apples-to-oranges ratio and the apples-to-kiwis ration in [fb].
IMPORTANT: YOU MUST USE [apples_to_oranges_ratio] AND
[apples_to_kiwis_ratio] FROM ABOVE.
Eg. ratio_sum fb1 => Ok 4
ratio_sum fb2 => Error
*)
let ratio_sum : fruit basket -> int result =
fun fb ->
match fb with
| [] -> assert false
| h::t -> 
                let extract_value = function
                        |Ok value -> value
                        |Error _ -> failwith "error"in
                
                              

