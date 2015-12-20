(*
 * env.ml
 *
 *     Environments and the values that they contain.
 *
 *)

(* Types. *)

type id = string

type value = 
   | Val_unit
   | Val_bool    of bool
   | Val_int     of int
   | Val_prim    of (value list -> value)      (* primitive functions *)
   | Val_lambda  of env * id list * Ast.expr list

and env = { parent: env option; bindings: (id, value) Hashtbl.t }

(* Values. *)

let string_of_value v =
   match v with
      | Val_unit       -> "#u"
      | Val_bool true  -> "#t"
      | Val_bool false -> "#f"
      | Val_int i      -> string_of_int i
      | Val_prim _     -> "[primitive function]"
      | Val_lambda _   -> "[lambda expression]"


(* Environments. *)

let make parent = { parent = parent; bindings = Hashtbl.create 5 }

let rec lookup env name =
   let { parent = p; bindings = b } = env in
   (*
    1. Look up name (id) in the bindings hash table
    2. If found, return the corresponding value
    3. O/w, search the parent environment
    4. If there's no parent environment, raise an exception
   *)
   try
      Hashtbl.find b name
   with Not_found ->
      begin
         match p with
         | Some p_env -> lookup p_env name
         | None -> let msg = Printf.sprintf "Lookup failed: %s not found" name in 
                failwith msg
         (*| None -> raise Not_found*)
      end

let add env name value = 
   let { parent = _; bindings = b } = env in
   begin
      Hashtbl.add b name value
   end

let add_all env names values = 
   let pairs = List.combine names values in
      List.iter (fun (x, y) -> add env x y) pairs


