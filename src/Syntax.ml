The library provides "@type ..." syntax extension and plugins like show, etc.
*)	*)
open GT 	open GT 


 open List

 (* Simple expressions: syntax and semantics *)	(* Simple expressions: syntax and semantics *)
module Expr =	module Expr =
  struct	  struct
@@ -34,14 +36,44 @@ module Expr =
    *)	    *)
    let update x v s = fun y -> if x = y then v else s y	    let update x v s = fun y -> if x = y then v else s y



     (* Some helping code for further work:
    boolToInt converts boolean b to integer
    intToBool converts integer i to boolean
    *)  
    let boolToInt b = if b then 1 else 0
    let intToBool i = i != 0


     (* Possible operations *)
    let operation oper leftExpr rightExpr = match oper with
        |"!!" -> boolToInt (( || ) (intToBool leftExpr) (intToBool rightExpr))
        |"&&" -> boolToInt (( && ) (intToBool leftExpr) (intToBool rightExpr))
        |"==" -> boolToInt (( == ) leftExpr rightExpr)
        |"!=" -> boolToInt (( != ) leftExpr rightExpr)
        |"<=" -> boolToInt (( <= ) leftExpr rightExpr)
        |"<" -> boolToInt (( <  ) leftExpr rightExpr)
        |">=" -> boolToInt (( >= ) leftExpr rightExpr)
        |">" -> boolToInt (( >  ) leftExpr rightExpr)
        |"+" -> ( +  ) leftExpr rightExpr
        |"-" -> ( -  ) leftExpr rightExpr
        |"*" -> ( *  ) leftExpr rightExpr
        |"/" -> ( /  ) leftExpr rightExpr
        |"%" -> ( mod ) leftExpr rightExpr


     (* Expression evaluator	    (* Expression evaluator
           val eval : state -> t -> int	          val eval : state -> t -> int
 	 
       Takes a state and an expression, and returns the value of the expression in 	       Takes a state and an expression, and returns the value of the expression in 
       the given state.	       the given state.
    *)	    *)
    let eval _ = failwith "Not implemented yet"	    let rec eval state expr = match expr with
    |Const cName -> cName
    |Var varName -> state varName
    |Binop (oper, leftExpr, rightExpr) -> 
        operation oper (eval state leftExpr) (eval state rightExpr)


   end	  end


 @@ -65,7 +97,11 @@ module Stmt =
        Takes a configuration and a statement, and returns another configuration	       Takes a configuration and a statement, and returns another configuration
    *)	    *)
    let eval _ = failwith "Not implemented yet"	    let rec eval (s, i, o) p = match p with
    | Read variable_name  -> (Expr.update variable_name  (hd i) s, tl i, o)
    | Write expression   -> (s, i, o @ [Expr.eval s expression])
    | Assign (variable_name, expression  ) -> (Expr.update variable_name (Expr.eval s expression ) s, i, o)
    | Seq (e1, e2)  -> eval (eval (s, i, o) e1) e2   


   end	  end