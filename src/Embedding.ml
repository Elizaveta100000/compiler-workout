Skip to content
 
Search or jump to…

Pull requests
Issues
Marketplace
Explore
 @Elizaveta100000 Sign out
0
0 84 Elizaveta100000/compiler-workout
forked from danyaberezun/compiler-workout
 Code  Pull requests 0  Projects 0  Wiki  Insights  Settings
compiler-workout/src/Embedding.ml
@Elizaveta100000 Elizaveta100000 Update Embedding.ml
81050ed  13 days ago
@Elizaveta100000 @danyaberezun
42 lines (32 sloc)  941 Bytes
    
(* A deep embedding of simple expressions in OCaml. *)

(* Opening GT yet again. *)
open GT
       
(* Opening the substrate module for convenience. *)
open Expr

(* Shortcuts for leaf constructors *)
let ( ! ) x = Var x
let ( !? ) n = Const n

(* Implementation of operators *)
let binop op x y = Binop (op, x, y)
                         
let ( +  ) = binop "+"
let ( -  ) = binop "-"
let ( *  ) = binop "*"
let ( /  ) = binop "/"
let ( %  ) = binop "%"
let ( <  ) = binop "<"
let ( <= ) = binop "<="
let ( >  ) = binop ">"
let ( >= ) = binop ">="
let ( == ) = binop "=="
let ( != ) = binop "!="
let ( && ) = binop "&&"
let ( || ) = binop "!!"

(* Some predefined names for variables *)
let x = !"x"
let y = !"y"
let z = !"z"
let t = !"t"
  (* Voila; comment this out before submitting the solution
let _ =
List.iter (fun e -> Printf.printf "eval s (%s) = %d\n" (show(expr) e) (eval s e)) [x+y*z- !?3; t-z+y && x]
*)


                   

