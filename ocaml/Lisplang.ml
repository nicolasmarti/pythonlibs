open Pycaml
open Nicerpy
open Libparser

module L = struct
    
  let ctxt = ref (Lisp.init_ctxt ())

  let saved_ctxt : (Lisp.env list) ref = ref []

  (* the name of the language *)
  let name = "Lisp"

  (* the error and exception, plus a format function *)
  type error = Lisp.lisp_error
  exception Exception = Lisp.LispException
      
  let error2string = Lisp.error2string

  (* the values and types *)
  type ty = unit
  type value = Lisp.expr

  (* functions to create a string from types and values *)
  let ty2string ty = ""
  let value2string = Lisp.expr2string

  (* initialization *)
  let init () = ctxt := Lisp.init_ctxt (); saved_ctxt := []

  (* equality over two values *)
  let eq_value v1 v2 = Lisp.eq v1 v2

  (* marshalling from/to python*)
  let rec marshal_to_python v = 
    match v with
      | Lisp.String s -> Some (string_to_py s)
      | Lisp.Int i -> Some (int_to_py i)
      | Lisp.Float f -> Some (float_to_py f)
      | Lisp.List l -> (
	let l' = List.fold_right (fun hd acc ->
	  match acc with 
	    | None -> None
	    | Some l' ->
	      match marshal_to_python hd with
		| None -> None
		| Some hd -> Some (hd::l')
	) l (Some []) in
	match l' with
	  | None -> None
	  | Some l' ->
	    Some (pylist_fromarray (Array.of_list l'))
      )
      | _ -> None


  (*
    TupleType
  | StringType
  | IntType
  | FloatType
  | ListType
  | BoolType
  | NoneType
  | CallableType
  | ModuleType
  | ClassType
  | TypeType
  | DictType
  | NullType
  | CamlpillType
  | OtherType
  | AnyType
  
  *)
  let rec marshal_from_python o = 
    match pytype o with
      | StringType -> Some (Lisp.String (py_to_string o))
      | IntType -> Some (Lisp.Int (py_to_int o))
      | FloatType -> Some (Lisp.Float (py_to_float o))
      | ListType -> (
	let a = Array.fold_right (fun hd acc ->
	  match acc with
	    | None -> None
	    | Some acc ->
	      match marshal_from_python hd with
		| None -> None
		| Some hd ->
		  Some (hd::acc)
	) (pylist_toarray o) (Some []) in
	match a with
	  | None -> None
	  | Some l -> Some (Lisp.Quoted (Lisp.List l))
      )
      | _ -> None

  (* application *)
  let apply f args = Lisp.eval (Lisp.List (f::(Array.to_list args))) !ctxt

  (* eval *)
  let eval s =
    let lines = stream_of_string s in
    let pb = build_parserbuffer lines in
    let e = (
      try
	Lisp.parse_expr pb 
      with
	| NoMatch -> raise (Lisp.LispException (Lisp.StringError (markerror pb)))
    ) in
    let s_ctxt = !ctxt in
    try (
      let res = Lisp.eval e !ctxt in
      ctxt := s_ctxt;
      res    
    ) with | e -> ctxt := s_ctxt; raise e

  let definition s = 
    let s_ctxt = Hashtbl.copy !ctxt in
    let lines = stream_of_string s in
    let pb = build_parserbuffer lines in
    let es = (
      try
	many1 Lisp.parse_expr pb 
      with
	| NoMatch -> 	  
	  raise (Lisp.LispException (Lisp.StringError (markerror pb)))
    ) in  
    try (
      let _ = List.map (fun hd -> Lisp.eval hd !ctxt) es in

      let defined = Array.of_list (
      (* val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c *)
      Hashtbl.fold (fun name expr acc ->
	if not (Hashtbl.mem s_ctxt name) then
	  (name, expr)::acc
	else acc
      ) !ctxt []
      ) in      
      saved_ctxt := s_ctxt::!saved_ctxt;
      pb.beginpointer, defined
    ) with | e -> ctxt := s_ctxt; raise e

  let undo_definition () = 
    ctxt := List.hd !saved_ctxt;
    saved_ctxt := List.tl !saved_ctxt

end
