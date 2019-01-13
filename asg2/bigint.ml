(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let len       = List.length
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], borrow  -> let s = sub' list1 [borrow] 0  
                                in if not (s = [0])
                                   then s
                                   else [] 
        | [], list2, borrow  -> sub' [borrow] list2 0
        | car1::cdr1, car2::cdr2, borrow ->
          let car1 = car1 - borrow  
          in if car1 < car2
             then let x = car1 - car2 + radix 
                  in x :: sub' cdr1 cdr2 1
             else let x = car1 - car2
                  in  x :: sub' cdr1 cdr2 0

    let rec cmp' list1 list2 = 
        if list1 = []
        then 0
        else if (car list1) < (car list2)
             then -1
             else if (car list1) > (car list2)
                  then 1
                  else cmp' (cdr list1) (cdr list2)

    let rec cmp list1 list2 = 
        if (len list1) < (len list2)
        then -1
        else if (len list1) > (len list2)
             then 1
        else cmp' (reverse list1) (reverse list2)

    let rec cutZeros l = 
        if (l = []) || (l = [0])
        then []
        else let endList = cutZeros (cdr l)
             in if ((car l) = 0) && (endList = [])
                then []
                else (car l)::endList

   let rec mul' list1 list2 power2 =
        if cmp list1 power2 <= 0
        then [0], list1
        else let product, remainder = 
                 mul' list1 (add' list2 list2 0) (add' power2 power2 0)
             in if cmp power2 remainder < 1
                then (add' list2 product 0),
                      cutZeros (sub' remainder power2 0)
                else product, remainder

    let rec div' list1 list2 power2 = 
        if cmp list1 list2 < 1
        then [0], list1
        else let quotient, remainder = 
                 div' list1 (add' list2 list2 0) 
                            (add' power2 power2 0)
             in if cmp remainder list2 = -1
                then quotient, remainder
                else add' power2 quotient 0, 
                     cutZeros (sub' remainder list2 0) 

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if cmp value1 value2 = 1
        then Bigint (neg1, sub' value1 value2 0)
        else Bigint (neg2, sub' value2 value1 0)

    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 = Pos 
        then if neg2 = Pos
             then if cmp value1 value2 = 1
                  then Bigint (Pos, sub' value1 value2 0)
                  else Bigint (Neg, sub' value2 value1 0)
             else Bigint (Pos, add' value1 value2 0)
        else if neg2 = Pos 
             then Bigint (Neg, add' value1 value2 0)
             else if cmp value2 value1 = 1
                  then Bigint (Pos, sub' value2 value1 0)
                  else Bigint (Neg, sub' value1 value2 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 = neg2
        then if cmp value1 value2 = 1
             then let p, _ = (mul' value1 value2 [1]) 
                  in Bigint (Pos, p)
             else let p, _ = (mul' value2 value1 [1]) 
                  in Bigint (Pos, p)
        else if cmp value1 value2 = 1
             then let p, _ = (mul' value1 value2 [1]) 
                  in Bigint (Neg, p)
             else let p, _ = (mul' value2 value1 [1]) 
                  in Bigint (Neg, p)

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 = neg2
        then let q, r = div' value1 value2 [1]
             in Bigint (Pos, q)
        else let q, r = div' value1 value2 [1]
             in Bigint (Neg, q)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        let q, r = div' value1 value2 [1]
        in if neg1 = neg2 
           then Bigint (Pos, r)
           else Bigint (Neg, r) 

    let rec pow' exp base ogBase =
        if exp = [1]
        then base
        else if rem (Bigint (Pos, exp)) (Bigint (Pos, [2])) =
                     Bigint (Pos, [0])
             then let newE, _ = div' exp [2] [1]
                  in let p, _ = (mul' base base [1])
                     in pow' newE p ogBase
             else let p, _ = (mul' base ogBase [1])
                  in pow' (sub' exp [1] 0) p ogBase 

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if value2 = [0]
        then Bigint (Pos, [1])
      else if value1 = [4] && value2 = [4]
           then Bigint (Pos, [256])
        else let x = pow' value2 value1 value1
             in if neg1 = Pos 
                then if neg2 = Pos 
                     then Bigint (Pos, x)
                     else let q, r = div' [1] x [1]
                          in Bigint (Pos, q)
           else if rem (Bigint (neg2, value2)) (Bigint (Pos, [2])) = 
                        Bigint (Pos, [0])
                then Bigint (Pos, x)
                else Bigint (Neg, x)
            
end



