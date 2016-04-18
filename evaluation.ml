(** A mini-ML
    @author Stuart M. Shieber

    This module implements a small untyped ML-like language under
    various operational semantics.
 *)

open Expr ;;
  
(* Exception for evaluator runtime generated by a runtime error *)
exception EvalError of string ;;
(* Exception for evaluator runtime generated by an explicit "raise" construct *)
exception EvalException ;;

module type Env_type = sig
    type env
    type value =
       | Val of expr
       | Closure of (expr * env)
    val create : unit -> env
    val close : expr -> env -> value
    val lookup : env -> varid -> value
    val extend : env -> varid -> value ref -> env
    val env_to_string : env -> string
    val value_to_string : ?printenvp:bool -> value -> string
  end

module Env : Env_type =
  struct

    type env = (varid * value ref) list
     and value =
       | Val of expr
       | Closure of (expr * env)

    exception EnvUnbound

    (* Creates an empty environment *)
    let create () : env = [] ;;

    (* Creates a closure from an expression and the environment it's
       defined in *)
    let close (exp: expr) (env: env) : value =
      failwith "close not implemented" ;;

    (* Looks up the value of a variable in the environment *)
    let lookup (env: env) (varname: varid) : value =
      failwith "lookup not implemented" ;;

    (* Returns a new environment just like env except that it maps the
       variable varid to loc *)
    let extend (env: env) (varname: varid) (loc: value ref) : env =
      failwith "extend not implemented" ;;

    (* Returns a printable string representation of an environment *)
    let env_to_string (env: env) : string =
      failwith "env_to_string not implemented" ;;

    (* Returns a printable string representation of a value; the flag
       printenvp determines whether to include the environment in the
       string representation when called on a closure *)
    let value_to_string ?(printenvp : bool = true) (v: value) : string =
      failwith "value_to_string not implemented" ;;
  end
;;
	     
(* The evaluation function: Returns the result of type `value` of
   evaluating the expression `exp` in the environment `env`. In this
   initial implementation, we just convert the expression unchanged to
   a value and return it. *)

  
let eval_t exp _env = Env.Val exp ;;
let eval_s _ = failwith "eval_s not implemented" ;;
let eval_d _ = failwith "eval_d not implemented" ;;
let eval_l _ = failwith "eval_l not implemented" ;;

let evaluate = eval_t ;;

