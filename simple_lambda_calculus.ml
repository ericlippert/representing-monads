(* I'm going through Filinski's paper "Representing Monads" and trying
to understand it by writing code. *)

module Expression = struct
  type t =
    | Identifier of string
    | Lambda of string * t
    | Apply of t * t
    | Let of string * t * t

  let ident s = Identifier s
  let lambda x b =
    match x with
    | Identifier x -> Lambda (x, b)
    | _ -> failwith "Lambda first parameter must be an identifier"

  let apply f a = Apply (f, a)
  let apply2 f a1 a2 = apply (apply f a1) a2
  let apply3 f a1 a2 a3 = apply (apply (apply f a1) a2) a3
  let let_ s e1 e2 =
    match s with
    | Identifier s -> Let (s, e1, e2)
    | _ -> failwith "Let first parameter must be an identifier"

  let if_ condition consequence alternative =
    let dummy = ident "dummy" in
    apply3 (ident "if") condition
      (lambda dummy consequence) (lambda dummy alternative)

  let rec to_string expr =
    match expr with
    | Identifier x -> x
    | Lambda (x, body) ->
      Printf.sprintf "%s => %s" x (to_string body)
    | Apply (func, arg) ->
      let f = to_string func in
      let f = match func with
      | Identifier _ -> f
      | _ -> "(" ^ f ^ ")" in
      let a = to_string arg in
      f ^ " " ^ a
    | Let (x, e1, e2) ->
      Printf.sprintf "let %s = %s in %s" x (to_string e1) (to_string e2)
end

module Value = struct
  type t =
    | Boolean of bool
    | Integer of int
    | Function of (t -> t)

  let to_string v =
    match v with
    | Boolean b -> if b then "true" else "false"
    | Integer i -> string_of_int i
    | Function _ -> "function"
end

module Environment = struct
  type t = (string * Value.t) list

  let empty = []

  let add environment name value =
    match name with
    | Expression.Identifier name -> (name, value) :: environment
    | _ -> failwith "add first argument must be an identifier"

  let lookup environment name =
    let finder (x, _v) =
      x = name in
    let (_x, v) = List.find finder environment in
    v
end

module Evaluator = struct
  open Expression
  open Value
  let rec evaluate environment expression =
    match expression with
    | Identifier x -> Environment.lookup environment x
    | Lambda (x, body) ->
      let f v =
        let new_env = Environment.add environment (Identifier x) v in
        evaluate new_env body in
      Function f
    | Apply (func, arg) ->
      let func = evaluate environment func in
      let arg = evaluate environment arg in
      begin
        match func with
        | Function f -> f arg
        | _ -> failwith "type error: cannot apply argument to non-function"
      end
    | Let (x, e1, e2) ->
      let e1 = evaluate environment e1 in
      let new_env = Environment.add environment (Identifier x) e1 in
      evaluate new_env e2
end

open Expression
open Value
open Environment
open Evaluator

let my_not v =
  match v with
  | Boolean b -> Boolean (not b)
  | _ -> failwith "type error: 'not' applied to non-boolean"

let my_add x =
  let adder y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x + y)
    | _ -> failwith "type error: 'add' applied to non-integer" in
  Function adder

let my_multiply x =
  let multiplier y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x * y)
    | _ -> failwith "type error: 'multiply' applied to non-integer" in
  Function multiplier

let my_less_than x =
  let comparer y =
    match (x, y) with
    | (Integer x, Integer y) -> Boolean (x < y)
    | _ -> failwith "type error: 'less_than' applied to non-integer" in
  Function comparer

let my_subtract x =
  let subtracter y =
    match (x, y) with
    | (Integer x, Integer y) -> Integer (x - y)
    | _ -> failwith "type error: 'subtract' applied to non-integer" in
  Function subtracter

let my_if c =
  let consequence x =
    let alternative y =
      match c, x, y with
      | (Boolean c, Function x, Function y) ->
        if c then x (Boolean true) else y (Boolean false)
      | _ -> failwith "type error: 'if' applied to non-boolean" in
    Function alternative in
  Function consequence

let () =
  let zero = ident "zero" in
  let one = ident "one" in
  let two = ident "two" in
  let six = ident "six" in

  let true_ = ident "true" in
  let false_ = ident "false" in

  let a = ident "a" in
  let b = ident "b" in
  let c = ident "c" in
  let f = ident "f" in
  let n = ident "n" in
  let r = ident "r" in
  let x = ident "x" in

  let add_ = ident "add" in
  let subtract = ident "subtract" in
  let multiply = ident "multiply" in
  let less_than = ident "less_than" in
  let not_ = ident "not" in
  let id_ = ident "id" in
  let fix_ = ident "fix" in
  let factorial_ = ident "factorial" in

  let env = empty in
  let env = add env zero (Integer 0) in
  let env = add env one (Integer 1) in
  let env = add env two (Integer 2) in
  let env = add env six (Integer 6) in
  let env = add env true_ (Boolean true) in
  let env = add env false_ (Boolean false) in
  let env = add env not_ (Function my_not) in
  let env = add env add_ (Function my_add) in
  let env = add env subtract (Function my_subtract) in
  let env = add env multiply (Function my_multiply) in
  let env = add env less_than (Function my_less_than) in
  let env = add env (ident "if") (Function my_if) in

  (* let id = x => x *)
  let id = lambda x x in
  let env = add env id_ (evaluate env id) in

  (* let fix = f => let b = r => a => (f (r r)) a in b b *)
  let fix =
    lambda f
      (let_ b
        (lambda r (lambda a (apply (apply f (apply r r)) a)))
        (apply b b)) in
  let env = add env fix_ (evaluate env fix) in

  (* let factorial =
    let c = f => n => if (less_than n 2) 1 (multiply n (f (subtract n 1))) in
    fix c *)

  let factorial =
    let_ c (lambda f (lambda n
        (if_ (apply2 less_than n two)
          one
          (apply2 multiply n (apply f (apply2 subtract n one))))))
      (apply fix_ c) in

  let env = add env factorial_ (evaluate env factorial) in

  let app = apply factorial_ six in
  Printf.printf "%s\n" (Value.to_string (evaluate env app));
