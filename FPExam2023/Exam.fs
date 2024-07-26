module Exam2023
open System.Collections.Generic
open System.Threading.Tasks

(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2023 = 
 *)

(* 1: Logic *)

    type prop =  
    | TT  
    | FF  
    | And of prop * prop  
    | Or of prop * prop
    
    let p1 = And(TT, FF)  
    let p2 = Or(TT, FF)  
    let p3 = And(Or(TT, And(TT, FF)), TT)  
    let p4 = And(Or(TT, And(TT, FF)), Or(FF, And(TT, FF)))

    
(* Question 1.1: Evaluation *)
    let rec eval input = 
        match input with 
        | TT -> true
        | FF -> false
        | And(p1,p2) -> (eval p1) && (eval p2)
        | Or (p1,p2) -> (eval p1) || (eval p2)
    
      
    
(* Question 1.2: Negation and implication *)
    let rec negate input = 
        match input with
        | TT -> FF
        | FF -> TT
        | And(p1, p2) -> Or (negate p1, negate p2)
        | Or (p1,p2) -> And (negate p1, negate p2)
 
    let rec implies p q = 
        Or (negate p, q)
    //implications is just "negate p1 V q"

(* Question 1.3: Bounded universal quantifiers *)
    let rec forall f lst =
        let rec aux acc = function
            | [] -> acc
            | x::xs -> aux (And (acc, f x)) xs
        aux TT lst
(* Question 1.4: Bounded existential quantifiers *)

    let exists f lst =
        lst
        |> List.map f
        |> List.fold (fun acc p -> Or (acc, p)) FF
    
(* Question 1.5: Bounded unique existential quantifiers *)

    let existsOne f lst =
        let rec aux acc = function
            | [] -> acc
            | x :: xs ->
                let conjunction = List.fold (fun acc' x' -> And (acc', negate (f x'))) TT (List.filter ((<>) x) lst)
                let disjunction = Or (acc, And (f x, conjunction))
                aux disjunction xs
        aux FF lst



    
(* 2: Code Comprehension *)
 
    let rec foo xs ys =  
        match xs, ys with  
        | _       , []                  -> Some xs   
        | x :: xs', y :: ys' when x = y -> foo xs' ys'   
        | _       , _                   -> None  
          
    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  

    let baz (a : string) (b : string) =  
        bar [for c in a -> c] [for c in b -> c] |>  
        List.fold (fun acc c -> acc + string c) ""

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
        Type: The function foo takes two lists as input arguments and returns an option type (Some xs or None).
        Type: The function bar also takes two lists as input arguments and returns a list.
        Type: The function baz takes two strings as input arguments and returns a string.  
        
     Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: Behavior: It recursively compares the elements of the input lists xs and ys. If the lists match (i.e., have the same elements in the same order), it returns Some xs. Otherwise, it returns None.

    Behavior: It repeatedly calls foo with the input lists xs and ys. If foo returns Some zs, it recursively calls bar with zs and ys. If foo returns None, it processes the remaining elements of xs and constructs a new list.

    Behavior: It converts the input strings a and b into lists of characters, then calls bar with these lists. The result is a concatenated string formed by folding over the resulting list.
    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A:
    Appropriate Name: A better name for bar could be processLists.
    Appropriate Name: Since foo checks for equality between elements of two lists, a more descriptive name could be listEquality.
    Appropriate Name: Given its behavior, concatenateStrings or stringConcatenation would be more fitting names for baz.
        
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `[for c in a -> c]`
    * B: `[for c in b -> c]`
    * C: `List.fold (fun acc c -> acc + string c) ""`

    Q: In the context of the baz function, i.e. assuming that `a` and `b` are strings, 
       what are the types of snippets A, B, and C and what are they -- 
       focus on what they do rather than how they do it.
    
    A:
        Snippet A:
        Type: This snippet is a list comprehension that iterates over each character c in the string a and constructs a list of characters.
        Behavior: It converts the string a into a list of its individual characters.
        Purpose: It prepares the input for further processing.
        Snippet B:
        Type: Similar to Snippet A, this is also a list comprehension that iterates over each character c in the string b and constructs a list of characters.
        Behavior: It converts the string b into a list of its individual characters.
        Purpose: Like Snippet A, it prepares the input for subsequent steps.
        Snippet C:
        Type: This snippet is a fold operation (List.fold) that combines the characters from the lists generated by Snippets A and B.
        Behavior: It concatenates the characters from the lists by applying the function (fun acc c -> acc + string c) to each character c and accumulating the result in the accumulator acc.
        Purpose: It creates a single string by joining the characters from both lists.
            
    Q: Explain the use of the `|>`-operator in the baz function.

    A:  The |> operator (often called the “pipe” operator) is used to chain functions together in F# (and other functional languages).
        In the baz function, it applies the result of Snippet B (the list of characters from string b) as an argument to the List.fold operation in Snippet C.
        Essentially, it allows you to pass the intermediate result from one operation directly to the next operation, making the code more concise and readable.

    *)

(* Question 2.3: No recursion *) 

    let foo2 xs ys =
        let checkEquality xs' ys' =
            List.forall2 (=) xs' ys'

        match List.splitAt (List.length ys) xs with
        | _, [] -> Some xs
        | xs', ys' when checkEquality xs' ys' -> Some xs'
        | _ -> None


(* Question 2.4 *)

    (*

    Q: The function `bar` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A:  Suppose we call bar [1; 2; 3] [3; 2; 1].
        The foo function is evaluated immediately to its final result (since we’re allowed to do so):
        foo [1; 2; 3] [3; 2; 1] evaluates to Some [1; 2; 3].
        Now let’s analyze the bar function:
        The first pattern match checks if foo xs ys returns Some zs. If so, it recursively calls bar zs ys.
        The second pattern match handles the case when foo xs ys returns None. It processes the remaining elements of xs and constructs a new list.
        The recursive call to bar occurs after the pattern match, which means it’s not the last operation in the function.
        Since the recursive call is not the last operation (due to the subsequent list construction), the bar function is not tail recursive.
        In summary, the bar function does not meet the criteria for tail recursion because the recursive call is followed by additional list manipulation

    *)
(* Question 2.5 *)

    // let barTail xs ys =
    //     let rec processRemaining xs cont =
    //         match xs with
    //         | [] -> cont []  // Base case: empty list
    //         | x :: xs' ->
    //             let handleResult zs =
    //                 match zs with
    //                 | Some result -> processRemaining result cont
    //                 | None -> processRemaining xs' (fun acc -> cont (x :: acc))
    //             foo xs ys handleResult
    //     processRemaining xs (fun acc -> acc)



(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)

    // let rec collatz n = 
    //     match n with 
    //     |1 -> [1]
    //     |_ when n % 2 = 0 -> n :: collatz (n/2)
    //     |_ -> n :: collatz (3 * n + 1)

    let collatz n =
        let rec loop acc n =
            match n with
            | 1 -> acc
            | _ when n % 2 = 0 -> loop (acc + 1) (n / 2)
            | _ -> loop (acc + 1) (3 * n + 1)
        loop 1 n

(* Question 3.2: Even and odd Collatz sequence elements *)

    let evenOddCollatz x = 
        let rec collatz n (even, odd) = 
            match n with
            | 1 -> (even, odd + 1) // so it counts the last
            | _ when n % 2 = 0 -> collatz (n/2) (even + 1, odd)
            | _ -> collatz (3 * n + 1) (even, odd + 1)
        let (even, odd) = collatz x (0,0)
        (even, odd)

(* Question 3.3: Maximum length Collatz Sequence *)
    // let collatz n =
    //     let rec loop acc n =
    //         match n with
    //         | 1 -> acc
    //         | _ when n % 2 = 0 -> loop (acc + 1) (n / 2)
    //         | _ -> loop (acc + 1) (3 * n + 1)
    //     loop 1 n

    let maxCollatz2 x y =
        let findMaxCollatz (maxNum, maxLength) num =
            let length = collatz num
            if length > maxLength then (num, length)
            else (maxNum, maxLength)
        
        Seq.init (y - x + 1) (fun i -> x + i)
        |> Seq.fold findMaxCollatz (x, collatz x)

(* Question 3.4: Collecting by length *)
    let collect1 x y =
        let map = Dictionary<int, HashSet<int>>()
        for i in x .. y do
            let length = collatz i
            if not (map.ContainsKey(length)) then
                map.[length] <- HashSet()
            map.[length].Add(i) |> ignore
        map |> Seq.map (fun kvp -> kvp.Key, kvp.Value |> Set.ofSeq) |> Map.ofSeq

    let collect x y =
        let numbersWithLengths = 
            [x .. y]
            |> List.map (fun n -> (collatz n, n))
        
        let groupedByLength = 
            numbersWithLengths
            |> List.fold (fun acc (length, number) ->
                acc
                |> Map.change length (function
                    | None -> Some (Set.singleton number)
                    | Some set -> Some (Set.add number set))
            ) Map.empty
        
        groupedByLength
(* Question 3.5: Parallel maximum Collatz sequence *)

    let parallelMaxCollatz x y n = 
        let step = (y - x + 1) / n
        let tasks = 
            [for i in 0 .. n - 1 do
                let start = x + i * step
                let end' = if i = n - 1 then y else start + step - 1
                yield Task.Run (fun () -> maxCollatz2 start end')]
        Task.WhenAll(tasks)
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> Array.fold (fun (maxNum, maxLength)(num, length) -> 
            if length > maxLength then (num, length) else (maxNum, maxLength)    
        ) (x, collatz x)
        |> fst
(* 4: Memory machines *)

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of expr             // Memory lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
          
    type stmnt =  
    | Assign of expr * expr      // Assign value to memory location
    | While  of expr * prog      // While loop
      
    and prog = stmnt list        // Programs are sequences of statements

    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    // Starting from memory {0, 0, 2, 0}
    let fibProg x =  
        [Num 0 .<-. Num x       // {x, 0, 2, 0}
         Num 1 .<-. Num 1       // {x, 1, 2, 0}
         Num 2 .<-. Num 0       // {x, 1, 0, 0}
         While (Lookup (Num 0), 
                [Num 0 .<-. Lookup (Num 0) .-. Num 1  
                 Num 3 .<-. Lookup (Num 1)  
                 Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)  
                 Num 2 .<-. Lookup (Num 3)  
                ]) // after loop {0, fib (x + 1), fib x, fib x}
         ]

(* Question 4.1: Memory blocks *)

    type mem = unit (* replace this entire type with your own *)
    let emptyMem _ = failwith "not implemented"
    let lookup _ = failwith "not implemented"
    let assign _ = failwith "not implemented"

(* Question 4.2: Evaluation *)

    let evalExpr _ = failwith "not implemented"
    let evalStmnt _ = failwith "not implemented"
    let evalProg _ = failwith "not implemented"
    
(* Question 4.3: State monad *)
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)  
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM m (SM f) = f m

    let lookup2 _ = failwith "not implemented"
    let assign2 _ = failwith "not implemented"

(* Question 4.4: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    let evalStmnt2 _ = failwith "not implemented"
    let evalProg2 _ = failwith "not implemented"
    
(* Question 4.5: Parsing *)
    
    open JParsec.TextParser
      
    let ParseExpr, eref = createParserForwardedToRef<expr>()  
    let ParseAtom, aref = createParserForwardedToRef<expr>()  
      
    let parseExpr _ = failwith "not implemented" // Parse addition and minus
          
    let parseAtom _ = failwith "not implemented" // Parse numbers and lookups

//    Uncomment the following two lines once you finish parseExpr and parseAtom             
//    do aref := parseAtom  
//    do eref := parseExpr  
      