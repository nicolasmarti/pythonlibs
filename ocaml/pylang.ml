open Pycaml
open Printf
open Lang_intf

module PyLang =
  functor (L: Lang) -> struct

    (* defined terms *)
    let defined = ref []
    
    (* debug flag *)
    let debug = ref false;;
    
    (* create the python module *)
    let mdl = pyimport_addmodule L.name;;
    let mdl_dict = pymodule_getdict mdl;;
    
    (* create the python class that will be used to store values *)
    let _ = python_exec (String.concat "" 
			   ["class Value"; L.name ;":
     # just register the id
     # the id should be already registered in ocaml
     def __init__(self, o):
         self.o=o

     # decref the term registered by id
     def __del__(self):
         try:
             ";L.name;".decref(self.o)
         except:
             return None

     # return the string representation
     def __str__(self):
         return ";L.name;".to_string(self.o)

     # application (here args is a tuple)
     def __call__(self, *args):
         return ";L.name;".apply(self.o, args)

def createValue";L.name;"(o):
    return Value";L.name;"(o)
"])

    (* grab the pyobjects *)
    let value_class = python_eval (String.concat "" ["Value"; L.name]);;
    let createValue_function = python_eval (String.concat "" ["createValue"; L.name]);;
    

    (* the wrapping *)
    let wrap_value (v: L.value) =
      if !debug then printf "registered %s\n" (L.value2string v);
      let res = pycallable_asfun createValue_function [| pywrap_value v |] in
      res
    ;;

    (* the unwrapping *)
    let unwrap_value (value: pyobject) : L.value =
      let isValue = pyobject_isinstance (value, value_class) in
      if isValue = 1 then
	let o = pyobject_getattr (value, pystring_fromstring "o") in
        pyunwrap_value o
      else
	raise (Failure "unwrap _value: not a value")
    
    (* the marshalling *)
    let marshal_value_python (v: L.value) =
      (* first let's try to marshall python value *)
      match L.marshal_to_python v with
	| Some o -> o
	| None -> wrap_value v
	  
    let marshal_python_value (o: pyobject) =
      (* first let's try to marshall python value *)
      match L.marshal_from_python o with
	| Some v -> v
	| None -> unwrap_value o

    (* now the function of the modul, called by the value object *)
    let _ = 
      python_interfaced_function 
	~register_as: (String.concat "" [L.name; ".apply"])
	~docstring:"application of a registered term with python arguments"
	[|AnyType; TupleType|]
	(fun args ->
	  match args with
	    | [| o; args |] ->
	       let args = pytuple_toarray args in
	       let args = Array.map (fun arg -> marshal_python_value arg) args in
	       try
		 let v = L.apply (pyunwrap_value o) args in
		 let o = marshal_value_python v in
		 o
	       with
	       | L.Exception err -> 
		  raise (Failure (L.error2string err))
	       | Failure s -> 
		  raise (Failure s)
	       | _ -> 
		    raise (Failure "_.apply: unknown exception")
	)
	
    ;;

    let _ = 
      python_interfaced_function 
	~register_as: (String.concat "" [L.name; ".to_string"])
	~docstring:"returns string representation of a registered term"
	[|AnyType|]
	(fun [| o |] ->
	  pystring_fromstring (L.value2string (pyunwrap_value o))
	)
    ;;


    let _ = 
      python_interfaced_function 
	~register_as: (String.concat "" [L.name; ".init"])
	~docstring:"initialize the context"
	[| |]
	(fun [| |] -> L.init (); pynone ())
    ;;
    
    let _ =
      python_interfaced_function 
      ~register_as: (String.concat "" [L.name; ".eval"])
      ~docstring:"enter a term"
      [|StringType|]
      (fun [| str |] ->
	let str = pystring_asstring str in
	try
	  let res = L.eval str in
	  marshal_value_python res	  
	with
	  | L.Exception err -> 
	    raise (Failure (L.error2string err))
      )
    ;;

    let _ =
      python_interfaced_function 
      ~register_as: (String.concat "" [L.name; ".definition"])
      ~docstring:"enter a definition"
      [|StringType|]
      (fun [| str |] ->
	let str = pystring_asstring str in
	try
	  let (consumed, defs) = L.definition str in
	  let names = Array.map (fun (hd1, hd2) ->
	    let pte = wrap_value hd2 in
	    let _ = pydict_setitemstring (mdl_dict, hd1, pte) in
	    hd1
	  ) defs in
	  defined := names::!defined;
	  pyint_fromint consumed	  
	with
	  | L.Exception err -> 
	    raise (Failure (L.error2string err))
      )
    ;;

    let _ =
      python_interfaced_function 
      ~register_as: (String.concat "" [L.name; ".undo_definition"])
      ~docstring:"undo previous definition"
      [| |]
      (fun [|  |] ->
	try
	  L.undo_definition (); 
	  let names = List.hd !defined in
	  defined := List.tl !defined;
	  Array.iter (fun hd -> ignore(pydict_delitemstring (mdl_dict, hd))) names;
	  pynone ()
	with
	  | L.Exception err -> 
	    raise (Failure (L.error2string err))
      )
    ;;
    
    (* finally import the module *)
    let _ = python_exec (String.concat "" ["import "; L.name]);;
    

end
