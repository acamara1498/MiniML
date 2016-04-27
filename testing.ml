open Expr ;;
open Evaluation ;;
open Printf ;;

type test = {label: string;
             content: bool Lazy.t;
             time: int;
             fail_msg: string} ;;

type status =
  | Passed
  | Failed of string
  | Raised_exn of string
  | Timed_out of int

exception Timeout ;;

let sigalrm_handler = Sys.Signal_handle (fun _ -> raise Timeout) ;;

let timeout (time : int) (delayed : 'a Lazy.t) : 'a =
  let old_behavior = Sys.signal Sys.sigalrm sigalrm_handler in
  let reset_sigalrm () = ignore (Unix.alarm 0);
    Sys.set_signal Sys.sigalrm old_behavior in
    ignore (Unix.alarm time) ;
  let res = Lazy.force delayed in
  reset_sigalrm () ; res ;;

let run_test ({label; time; content; fail_msg} : test)
             (continue : string -> status -> unit)
             : unit =
    try
      if timeout time content
      then continue label Passed
      else continue label (Failed fail_msg)
    with
    | Timeout -> continue label (Timed_out time)
    | exn     -> continue label
                   (Raised_exn (Printexc.to_string exn))

let present label status =
  match status with
  | Passed -> printf "%s: passed\n" label
  | Failed msg -> printf "%s: failed %s\n" label msg
  | Timed_out secs -> printf "%s: timed out in %d\n" label secs
  | Raised_exn msg -> printf "%s: raised %s\n" label msg ;;

let report (tests: test list) : unit =
  List.iter (fun test -> run_test test present) tests ;;

let test ?(fail_msg="somehow") ?(time=5) label content =
  {label = label;
   content = content;
   fail_msg = fail_msg;
   time = time} ;;

(* testing examples *)
let tests =
  [ test "should fail" (lazy (3 > 4)) ;
    test "should pass" (lazy (4 > 3)) ;
    test "should time out" (lazy (let rec f x = f x in f 1)) ;
    test "should raise exception" (lazy ((List.nth [0;1] 3) = 3))
  ] ;;
(* uncomment and execute to test framework *)
(* report tests ;; *)



(* **************TESTING************** *)
let addition = Binop("+", Num(3), Num(4)) ;;
let subtractions = Binop("-", Binop("-", Num(3), Num(4)), Num(5)) ;;
let mult = Binop("*", Num(5), Num(3)) ;;
let division = Binop("/", Binop("+", Num(3), Num(2)),
                          Binop("-", Num(6), Num(1))) ;;
let fun1 = Let("f", Fun("x", Var("x")), App(App(Var("f"), Var("f")), Num(3))) ;;
let let_rec = Letrec("f", Fun("x", Binop("+", Var("x"), Num(1))),
                        App(Var("f"), App(Var("f"), App(Var("f"), Num(5))))) ;;
let env_test = Let("x", Num(1), Let("f", Fun("y",
                                         Binop("+", Var("x"), Var("y"))),
               Let("x", Num(2), App(Var("f"), Num(3))))) ;;

print_string "testing substitution:\n\n" ;;

let tests_sub =
  [ test "addition " (lazy ((eval addition) = Num(7))) ;
    test "subtractions " (lazy ((eval subtractions) = Num(-6))) ;
    test "multiplication " (lazy ((eval mult) = Num(15))) ;
    test "division and parenthesis " (lazy ((eval division) = Num(1))) ;
    test "let fun " (lazy ((eval fun1) = Num(3))) ;
    test "let rec fun " (lazy ((eval let_rec) = Num(8))) ;
    test "environment (for subst) test " (lazy ((eval env_test) = Num(4)))
  ] ;;

report tests_sub ;;

print_string "...done testing substitution\n\n" ;;

print_string "testing dynamic environment\n\n" ;;
let env = Evaluation.Env.create () ;;
let tests_dynamic =
  [ test "addition " (lazy (match evald addition env with
                            | Env.Val p -> p = Num(7)
                            | _ -> false)) ;
    test "subtractions " (lazy (match evald subtractions env with
                            | Env.Val p -> p = Num(-6)
                            | _ -> false)) ;
    test "multiplication " (lazy (match evald mult env with
                            | Env.Val p -> p = Num(15)
                            | _ -> false)) ;
    test "division and parenthesis " (lazy (match evald division env with
                            | Env.Val p -> p = Num(1)
                            | _ -> false)) ;
    test "let fun " (lazy (match evald fun1 env with
                            | Env.Val p -> p = Num(3)
                            | _ -> false)) ;
    test "let rec fun " (lazy (match evald let_rec env with
                            | Env.Val p -> p = Num(8)
                            | _ -> false)) ;
    test "environment test " (lazy (match evald env_test env with
                            | Env.Val p -> p = Num(5)
                            | _ -> false))
    ] ;;

report tests_dynamic ;;

print_string "...done testing dynamic environment\n\n" ;;

print_string "testing lexical environment\n\n" ;;
let env = Evaluation.Env.create () ;;
let tests_lexical =
  [ test "addition " (lazy (match eval_lex addition env with
                            | Env.Val p -> p = Num(7)
                            | _ -> false)) ;
    test "subtractions " (lazy (match eval_lex subtractions env with
                            | Env.Val p -> p = Num(-6)
                            | _ -> false)) ;
    test "multiplication " (lazy (match eval_lex mult env with
                            | Env.Val p -> p = Num(15)
                            | _ -> false)) ;
    test "division and parenthesis " (lazy (match eval_lex division env with
                            | Env.Val p -> p = Num(1)
                            | _ -> false)) ;
    test "let fun " (lazy (match eval_lex fun1 env with
                            | Env.Val p -> p = Num(3)
                            | _ -> false)) ;
    test "let rec fun " (lazy (match eval_lex let_rec env with
                            | Env.Val p -> p = Num(8)
                            | _ -> false)) ;
    test "environment test " (lazy (match eval_lex env_test env with
                            | Env.Val p -> p = Num(4)
                            | _ -> false))
    ] ;;

report tests_lexical ;;

print_string "...done testing lexical environment\n\n" ;;