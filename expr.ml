
(** Abstract syntax of MiniML expressions *)

type expr =
  | Var of varid                         (* variables *)
  | Num of int                           (* integers *)
  | Bool of bool                         (* booleans *)
  | Unop of varid * expr                 (* unary operators *)
  | Binop of varid * expr * expr         (* binary operators *)
  | Conditional of expr * expr * expr    (* if then else *)
  | Fun of varid * expr                  (* function definitions *)
  | Let of varid * expr * expr           (* local naming *)
  | Letrec of varid * expr * expr        (* recursive local naming *)
  | Raise                                (* exceptions *)
  | Unassigned                           (* (temporarily) unassigned *)
  | App of expr * expr                   (* function applications *)
 and varid = string ;;

(** Sets of varids *)
module SS = Set.Make(struct
		      type t = varid
		      let compare = String.compare
		    end);;

type varidset = SS.t ;;

(** Test to see if two sets have the same elements (for
    testing purposes) *)
let same_vars = SS.equal;;

(** Generate a set of variable names from a list of strings (for
    testing purposes) *)
let vars_of_list = SS.of_list ;;

(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  failwith "free_vars not implemented" ;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let new_varname () : varid =
  failwith "new_varname not implemented" ;;

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  failwith "subst not implemented" ;;

(** Returns a string representation of the expr *)
let rec exp_to_string (exp: expr) : string =
    match exp with
  	| Var x -> "Var(" ^ x ^ ")"
  	| Num n -> "Num(" ^ string_of_int n ^ ")"
  	| Bool b -> "Bool(" ^ string_of_bool b ^ ")"
  	| Unop (x, e) -> "Unop(" ^ x ^ ", " ^ (exp_to_string e) ^ ")"
  	| Binop (x, e1, e2) -> "Binop(" ^ x ^ ", " ^ (exp_to_string e1) ^
  	                       ", " ^ (exp_to_string e2) ^ ")"
  	| Conditional (e1, e2, e3) -> "Conditional(" ^ (exp_to_string e1) ^ ", " ^
  	                              (exp_to_string e2) ^ ", " ^
  	                              (exp_to_string e3) ^ ")"
  	| Fun (x, e) -> "Fun(" ^ x ^ ", " ^ (exp_to_string e) ^ ")"
  	| Let (x, e1, e2) -> "Let(" ^ x ^ ", " ^ (exp_to_string e1) ^
  	                     ", " ^ (exp_to_string e2) ^ ")"
  	| Letrec (x, e1, e2) -> "Letrec(" ^ x ^ ", " ^ (exp_to_string e1) ^
  	                        ", " ^ (exp_to_string e2) ^ ")"
  	| Raise -> "Raise"
  	| Unassigned -> "Unassigned"
  	| App (e1, e2) -> "App(" ^ (exp_to_string e1) ^ ", " ^ (
  	                            exp_to_string e2) ^ ")"
