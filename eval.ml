(*
 * eval.ml
 *
 *     Evaluator.
 *
 *)

open Env

exception Type_error of string

let rec eval ast env =
   match ast with
      | Ast.Expr_unit    -> Val_unit
      | Ast.Expr_bool b  -> Val_bool b
      | Ast.Expr_int  i  -> Val_int i
      | Ast.Expr_id   id -> lookup env id
      | Ast.Expr_define (id, e) ->
           begin
              let value = eval e env in
              add env id value;
              Val_unit
           end
      | Ast.Expr_if (test_e, then_e, else_e) ->
           begin
             let test_result = eval test_e env in
              match test_result with
              | Val_bool true -> eval then_e env
              | Val_bool false -> eval else_e env
              | _ -> raise (Type_error "Cannot evaluate non-boolean in if statement")
           end
      | Ast.Expr_lambda (ids, exprs) -> Val_lambda (env, ids, exprs)
      | Ast.Expr_apply (e, es) -> begin
           let values = List.map (fun x -> eval x env) es in
           let f = eval e env in
              match f with
                 | Val_prim prim_func -> prim_func values
                 | Val_lambda (env', ids, exprs) -> begin
                      let parent_env = make (Some env') in
                      add_all parent_env ids values;
                      let results = List.map (fun x -> eval x parent_env) exprs in
                      List.hd (List.rev results);
                   end
                 | _ -> raise (Type_error "Cannot apply non-function")
        end
