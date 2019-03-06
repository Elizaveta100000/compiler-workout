open GT       


 open List

 (* The type for the stack machine instructions *)	(* The type for the stack machine instructions *)
@type insn =	@type insn =
(* binary operator                 *) | BINOP of string	(* binary operator                 *) | BINOP of string
@@ -23,7 +25,17 @@ type config = int list * Syntax.Stmt.config
    Takes a configuration and a program, and returns a configuration as a result	   Takes a configuration and a program, and returns a configuration as a result
 *)                         	 *)                         
let eval _ = failwith "Not yet implemented"	 let rec eval cfg prg =
  let step (st, (s, i, o)) p = match p with
    | BINOP op -> (Syntax.Expr.operation op (hd (tl st)) (hd st) :: (tl (tl st)), (s, i, o))
    | CONST n  -> (n :: st, (s, i, o))
    | READ     -> (hd i :: st, (s, tl i, o))
    | WRITE    -> (tl st, (s, i, o @ [hd st]))
    | LD variable_name    -> (s variable_name :: st, (s, i, o))
    | ST variable_name    -> (tl st, (Syntax.Expr.update variable_name (hd st) s, i, o))
  in match prg with
    | [] -> cfg
    | p :: ps -> eval (step cfg p) ps


 (* Top-level evaluation	(* Top-level evaluation
 @@ -41,4 +53,13 @@ let run i p = let (_, (_, _, o)) = eval ([], (Syntax.Expr.empty, i, [])) p in o
   stack machine	   stack machine
 *)	 *)


 let compile _ = failwith "Not yet implemented"	let rec compile_expr e = match e with
    | Syntax.Expr.Const n -> [CONST n]
    | Syntax.Expr.Var v -> [LD v]
    | Syntax.Expr.Binop (op, l_e,r_e) -> compile_expr l_e@ compile_expr r_e@ [BINOP op]

 let rec compile p = match p with
    | Syntax.Stmt.Read variable_name -> [READ; ST variable_name]
    | Syntax.Stmt.Write expression  -> compile_expr expression @ [WRITE]
    | Syntax.Stmt.Assign (variable_name, expression) -> compile_expr expression@ [ST variable_name]
    | Syntax.Stmt.Seq (e1, e2) -> compile e1 @ compile e2;;