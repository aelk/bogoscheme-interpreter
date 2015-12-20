(*
 * main.ml
 *
 *     Entry point to the bogoscheme interpreter.
 *
 *)

let run_program infile =
   let lexbuf = Lexing.from_channel infile in
   let env     = Env.make None in
   let rec loop env =
      let sexpr  = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = Ast.ast_of_sexpr s in
                 let _ = Eval.eval expr env in
                    loop env
   in
      Primitives.load env;
      loop env


(* Entry point of the interpreter. *)
let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         begin
            try
               run_program infile
            with e ->
               begin
                  match e with
                     | Failure f ->
                          Printf.fprintf stderr "\nERROR: %s\n" f
                     | Eval.Type_error s ->
                          Printf.fprintf stderr "\nERROR: type error: %s\n" s
                     | Not_found ->
                          Printf.fprintf stderr "\nERROR: name not found\n"
                     | _ ->
                          Printf.fprintf stderr "\nERROR: unspecified\n"
               end;
               close_in infile;
               exit 1
         end;
         close_in infile;
         exit 0


