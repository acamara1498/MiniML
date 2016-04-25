
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

(* Help function to print set *)
let print = SS.iter (fun x  -> Printf.printf "{%s}, " x) ;;

(** Return a set of the variable names free in [exp] *)
let rec free_vars (exp : expr) : varidset =
  match exp with
  | Var x -> SS.singleton x
  | Num _ | Bool _ -> SS.empty
  | Unop (x, e) -> free_vars e
  | Binop (x, e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Conditional (e1, e2, e3) -> SS.union (free_vars e1)
                                (SS.union (free_vars e2) (free_vars e3))
  | Fun (x, e) -> SS.remove x (free_vars e)
  | Let (x, e1, e2) -> SS.remove x (SS.union (free_vars e1) (free_vars e2))
  | Letrec (x, e1, e2) -> SS.remove x (SS.union (free_vars e1) (free_vars e2))
  | App (e1, e2) -> SS.union (free_vars e1) (free_vars e2)
  | Raise | Unassigned -> SS.empty
;;

(** Return a fresh variable, constructed with a running counter a la
    gensym. Assumes no variable names use the prefix "var". *)
let counter = ref ~-1 ;;
let new_varname () : varid =
    "newvar_" ^ string_of_int (counter := !counter + 1; !counter) ;;

(** Substitute [repl] for free occurrences of [var_name] in [exp] *)
let rec subst (var_name: varid) (repl: expr) (exp: expr) : expr =
  let rec sub (exp: expr) : expr =
    match exp with
    | Num _ -> exp
    | Bool _ -> exp
    | Var x -> if x = var_name then repl else exp
    | Unop (op, e) -> Unop (op, sub e)
    | Binop (op, e1, e2) -> Binop (op, sub e1, sub e2)
    | Conditional (e1, e2, e3) -> Conditional (sub e1, sub e2, sub e3)
    | Fun (y, body) ->
        if y = var_name then exp
        else
          if SS.mem y (free_vars repl)
          then
            let z = new_varname () in
            Fun(z, Fun(y, subst z body repl))
          else
            Fun(y, sub body)
    | Let (y, def, body) ->
        if y = var_name
        then Let (y, sub def, body)
        else Let (y, sub def, sub body)
    | Letrec (y, def, body) ->
        if y = var_name
        then Letrec (y, def, body)
        else Letrec (y, sub def, sub body)
    | Raise | Unassigned -> exp
    | App (e1, e2) -> App (sub e1, sub e2) in
    sub exp
;;

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
