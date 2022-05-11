(* open Ast.Asttypes *)
open Parsing.Parsetree

type level = int [@@deriving show]

let generic_level = 100000000 (* as in OCaml typing/btype.ml *)

let marked_level = -1 (* for marking a node, to check for cycles *)

type typ =
  | TVar of tv ref
  | TArrow of typ * typ * levels
  | Tconstr_integer of level
  | Tconstr_boolean of level
  | Tconstr_string of level
[@@deriving show]

and tv = Unbound of string * level | Link of typ [@@deriving show]

and levels = { mutable level_old : level; mutable level_new : level }
[@@deriving show]

let rec repr : typ -> typ = function
  | TVar ({ contents = Link t } as tvr) ->
      let t = repr t in
      tvr := Link t;
      t
  | t -> t

let get_level : typ -> level = function
  | TVar { contents = Unbound (_, l) } -> l
  | TArrow (_, _, ls) -> ls.level_new
  | Tconstr_boolean l | Tconstr_integer l | Tconstr_string l -> l
  | _ -> assert false

let gensym_counter = ref 0

let reset_gensym : unit -> unit = fun () -> gensym_counter := 0

let gensym : unit -> string =
 fun () ->
  let n = !gensym_counter in
  let () = incr gensym_counter in
  if n < 26 then String.make 1 (Char.chr (Char.code 'a' + n))
  else "t" ^ string_of_int n

let current_level = ref 1

let reset_level () = current_level := 1

let reset_type_variables () =
  reset_gensym ();
  reset_level ()

let enter_level () = incr current_level

let leave_level () = decr current_level

let newvar : unit -> typ =
 fun () -> TVar (ref (Unbound (gensym (), !current_level)))

let new_arrow : typ -> typ -> typ =
 fun ty1 ty2 ->
  TArrow (ty1, ty2, { level_new = !current_level; level_old = !current_level })

let rec cycle_free : typ -> unit = function
  | Tconstr_boolean _ | Tconstr_integer _ | Tconstr_string _ -> ()
  | TVar { contents = Unbound _ } -> ()
  | TVar { contents = Link ty } -> cycle_free ty
  | TArrow (_, _, ls) when ls.level_new = marked_level ->
      failwith "occurs check"
  | TArrow (t1, t2, ls) ->
      let level = ls.level_new in
      ls.level_new <- marked_level;
      cycle_free t1;
      cycle_free t2;
      ls.level_new <- level

let to_be_level_adjusted = ref []

let reset_level_adjustment () = to_be_level_adjusted := []

let update_level : level -> typ -> unit =
 fun l -> function
  | TVar ({ contents = Unbound (n, l') } as tvr) ->
      assert (not (l' = generic_level));
      if l < l' then tvr := Unbound (n, l)
  | TArrow (_, _, ls) as ty ->
      assert (not (ls.level_new = generic_level));
      if ls.level_new = marked_level then failwith "occurs check";
      if l < ls.level_new then (
        if ls.level_new = ls.level_old then
          to_be_level_adjusted := ty :: !to_be_level_adjusted;
        ls.level_new <- l)
  | Tconstr_boolean _ | Tconstr_integer _ | Tconstr_string _ -> ()
  | _ -> assert false

let rec unify : typ -> typ -> unit =
 fun t1 t2 ->
  if t1 = t2 then ()
  else
    match (repr t1, repr t2) with
    | ( (TVar ({ contents = Unbound (_, l1) } as tv1) as t1),
        (TVar ({ contents = Unbound (_, l2) } as tv2) as t2) ) ->
        if l1 > l2 then tv1 := Link t2 else tv2 := Link t1
    | TVar ({ contents = Unbound (_, l) } as tv), t'
    | t', TVar ({ contents = Unbound (_, l) } as tv) ->
        update_level l t';
        tv := Link t'
    | TArrow (tyl1, tyl2, ll), TArrow (tyr1, tyr2, lr) ->
        if ll.level_new = marked_level || lr.level_new = marked_level then
          failwith "cycle: occurs check";
        let min_level = min ll.level_new lr.level_new in
        ll.level_new <- marked_level;
        lr.level_new <- marked_level;
        unify_lev min_level tyl1 tyr1;
        unify_lev min_level tyl2 tyr2;
        ll.level_new <- min_level;
        lr.level_new <- min_level
    | _ -> failwith "unification error"

and unify_lev l ty1 ty2 =
  let ty1 = repr ty1 in
  update_level l ty1;
  unify ty1 ty2

type env = (varname * typ) list ref

let force_delayed_adjustments () =
  let rec loop acc level ty =
    match repr ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when l > level ->
        tvr := Unbound (name, level);
        acc
    | TArrow (_, _, ls) when ls.level_new = marked_level ->
        failwith "occurs check"
    | TArrow (_, _, ls) as ty ->
        if ls.level_new > level then ls.level_new <- level;
        adjust_one acc ty
    | _ -> acc
  and adjust_one acc = function
    | TArrow (_, _, ls) as ty when ls.level_old <= !current_level -> ty :: acc
    | TArrow (_, _, ls) when ls.level_old = ls.level_new -> acc
    | TArrow (ty1, ty2, ls) ->
        let level = ls.level_new in
        ls.level_new <- marked_level;
        let acc = loop acc level ty1 in
        let acc = loop acc level ty2 in
        ls.level_new <- level;
        ls.level_old <- level;
        acc
    | _ -> assert false
  in
  to_be_level_adjusted := List.fold_left adjust_one [] !to_be_level_adjusted

let gen : typ -> unit =
 fun ty ->
  force_delayed_adjustments ();
  let rec loop ty =
    match repr ty with
    | TVar ({ contents = Unbound (name, l) } as tvr) when l > !current_level ->
        tvr := Unbound (name, generic_level)
    | TArrow (ty1, ty2, ls) when ls.level_new > !current_level ->
        let ty1 = repr ty1 and ty2 = repr ty2 in
        loop ty1;
        loop ty2;
        let l = max (get_level ty1) (get_level ty2) in
        ls.level_old <- l;
        ls.level_new <- l
    | _ -> ()
  in
  loop ty

let inst : typ -> typ =
  let rec loop subst = function
    | TVar { contents = Unbound (name, l) } when l = generic_level -> (
        try (List.assoc name subst, subst)
        with Not_found ->
          let tv = newvar () in
          (tv, (name, tv) :: subst))
    | TVar { contents = Link ty } -> loop subst ty
    | TArrow (ty1, ty2, ls) when ls.level_new = generic_level ->
        let ty1, subst = loop subst ty1 in
        let ty2, subst = loop subst ty2 in
        (new_arrow ty1 ty2, subst)
    | ty -> (ty, subst)
  in
  fun ty -> fst (loop [] ty)

let rec typeof : env -> expression -> typ =
 fun env -> function
  | Pconstant (_, c) -> (
      match c with
      | Pconst_integer _ -> Tconstr_integer generic_level
      | Pconst_boolean _ -> Tconstr_boolean generic_level
      | Pconst_string _ -> Tconstr_string generic_level)
  | PVar (_, x) -> inst (List.assoc x !env)
  | PLam (_, x, e) ->
      let ty_x = newvar () in
      env := (x, ty_x) :: !env;
      let ty_e = typeof env e in
      new_arrow ty_x ty_e
  | PApp (_, e1, e2) ->
      let ty_fun = typeof env e1 in
      let ty_arg = typeof env e2 in
      let ty_res = newvar () in
      unify ty_fun (new_arrow ty_arg ty_res);
      ty_res
  | PLet (_, x, e, e2) ->
      enter_level ();
      let ty_e = typeof env e in
      leave_level ();
      gen ty_e;
      env := (x, ty_e) :: !env;
      typeof env e2
  | PBinOp (_, _, e1, e2) ->
      let ty_e1 = typeof env e1 in
      let ty_e2 = typeof env e2 in
      unify ty_e1 ty_e2;
      ty_e1

(* Type-check the top-level expression *)
let top_type_check : expression -> typ =
 fun expr ->
  reset_type_variables ();
  reset_level_adjustment ();
  let ty = typeof { contents = [] } expr in
  cycle_free ty;
  ty

(* expr: fun x -> x *)
let id = PLam (Lexing.dummy_pos, "x", PVar (Lexing.dummy_pos, "x"))

(* fun x -> fun y -> x y *)
let c1 =
  PLam
    ( Lexing.dummy_pos,
      "x",
      PLam
        ( Lexing.dummy_pos,
          "y",
          PApp
            ( Lexing.dummy_pos,
              PVar (Lexing.dummy_pos, "x"),
              PVar (Lexing.dummy_pos, "y") ) ) )

(* let (TArrow
         ( TVar { contents = Unbound ("a", 1) },
           TVar { contents = Unbound ("a", 1) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check id

   let (TArrow
         ( TVar
             {
               contents =
                 Link
                   (TArrow
                     ( TVar { contents = Unbound ("b", 1) },
                       TVar { contents = Unbound ("c", 1) },
                       { level_old = 1; level_new = 1 } ));
             },
           TArrow
             ( TVar { contents = Unbound ("b", 1) },
               TVar { contents = Unbound ("c", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check c1

   let (TArrow
         ( TArrow
             ( TVar { contents = Unbound ("d", 1) },
               TVar { contents = Unbound ("e", 1) },
               { level_old = 1; level_new = 1 } ),
           TArrow
             ( TVar { contents = Unbound ("d", 1) },
               TVar { contents = Unbound ("e", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLet
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            Some TEBool,
            c1,
            PVar (Lexing.dummy_pos, Var_name.of_string "x") ))

   let (TArrow
         ( TVar { contents = Unbound ("b", 1) },
           TVar { contents = Unbound ("b", 1) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLet
          ( Lexing.dummy_pos,
            Var_name.of_string "y",
            Some TEInt,
            PLam
              ( Lexing.dummy_pos,
                Var_name.of_string "z",
                PVar (Lexing.dummy_pos, Var_name.of_string "z") ),
            PVar (Lexing.dummy_pos, Var_name.of_string "y") ))

   let (TArrow
         ( TVar { contents = Unbound ("a", 1) },
           TArrow
             ( TVar { contents = Unbound ("c", 1) },
               TVar { contents = Unbound ("c", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLam
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    PVar (Lexing.dummy_pos, Var_name.of_string "z") ),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))

   let (TArrow
         ( TVar { contents = Link (TVar { contents = Unbound ("c", 1) }) },
           TVar { contents = Link (TVar { contents = Unbound ("c", 1) }) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLam
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    PVar (Lexing.dummy_pos, Var_name.of_string "z") ),
                PApp
                  ( Lexing.dummy_pos,
                    PVar (Lexing.dummy_pos, Var_name.of_string "y"),
                    PVar (Lexing.dummy_pos, Var_name.of_string "x") ) ) ))
   ;;

   try
     let _ =
       top_type_check
         (PLam
            ( Lexing.dummy_pos,
              Var_name.of_string "x",
              PApp
                ( Lexing.dummy_pos,
                  PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                  PVar (Lexing.dummy_pos, Var_name.of_string "x") ) ))
     in
     assert false
   with Failure e -> print_endline e
   ;;

   try
     let _ =
       top_type_check
         (PLet
            ( Lexing.dummy_pos,
              Var_name.of_string "x",
              Some TEInt,
              PVar (Lexing.dummy_pos, Var_name.of_string "x"),
              PVar (Lexing.dummy_pos, Var_name.of_string "x") ))
     in
     assert false
   with Not_found -> print_endline "unbound var"
   ;;

   (* Max Heiber's example *)
   (* fun y -> y (fun z -> y z) *)
   try
     let _ =
       top_type_check
         (PLam
            ( Lexing.dummy_pos,
              Var_name.of_string "y",
              PApp
                ( Lexing.dummy_pos,
                  PVar (Lexing.dummy_pos, Var_name.of_string "y"),
                  PLam
                    ( Lexing.dummy_pos,
                      Var_name.of_string "z",
                      PApp
                        ( Lexing.dummy_pos,
                          PVar (Lexing.dummy_pos, Var_name.of_string "y"),
                          PVar (Lexing.dummy_pos, Var_name.of_string "z") ) ) ) ))
     in
     assert false
   with Failure e -> print_endline e

   (* id can be `self-applied', on the surface of it *)
   let (TVar
         {
           contents =
             Link
               (TArrow
                 ( TVar { contents = Unbound ("c", 1) },
                   TVar { contents = Unbound ("c", 1) },
                   { level_old = 1; level_new = 1 } ));
         }) =
     top_type_check
       (PLet
          ( Lexing.dummy_pos,
            Var_name.of_string "id",
            Some TEInt,
            id,
            PApp
              ( Lexing.dummy_pos,
                PVar (Lexing.dummy_pos, Var_name.of_string "id"),
                PVar (Lexing.dummy_pos, Var_name.of_string "id") ) ))

   let (TArrow
         ( TVar { contents = Unbound ("i", 1) },
           TVar { contents = Unbound ("i", 1) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLet
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            Some TEInt,
            c1,
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLet
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    Some TEInt,
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        id ),
                    PVar (Lexing.dummy_pos, Var_name.of_string "z") ),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))

   (*
   fun x -> fun y -> let x = x y in fun x -> y x;;
   - : (('a -> 'b) -> 'c) -> ('a -> 'b) -> 'a -> 'b = <fun>
   *)
   let (TArrow
         ( TVar
             {
               contents =
                 Link
                   (TArrow
                     ( TVar
                         {
                           contents =
                             Link
                               (TArrow
                                 ( TVar { contents = Unbound ("d", 1) },
                                   TVar { contents = Unbound ("e", 1) },
                                   { level_old = 1; level_new = 1 } ));
                         },
                       TVar { contents = Unbound ("c", 1) },
                       { level_old = 1; level_new = 1 } ));
             },
           TArrow
             ( TVar
                 {
                   contents =
                     Link
                       (TArrow
                         ( TVar { contents = Unbound ("d", 1) },
                           TVar { contents = Unbound ("e", 1) },
                           { level_old = 1; level_new = 1 } ));
                 },
               TArrow
                 ( TVar { contents = Unbound ("d", 1) },
                   TVar { contents = Unbound ("e", 1) },
                   { level_old = 1; level_new = 1 } ),
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLam
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                PLet
                  ( Lexing.dummy_pos,
                    Var_name.of_string "x",
                    Some TEInt,
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        PVar (Lexing.dummy_pos, Var_name.of_string "y") ),
                    PLam
                      ( Lexing.dummy_pos,
                        Var_name.of_string "x",
                        PApp
                          ( Lexing.dummy_pos,
                            PVar (Lexing.dummy_pos, Var_name.of_string "y"),
                            PVar (Lexing.dummy_pos, Var_name.of_string "x") ) ) )
              ) ))

   (* now sound generalization ! *)
   let (TArrow
         ( TVar { contents = Unbound ("a", 1) },
           TVar { contents = Unbound ("a", 1) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))

   (* now sound generalization ! *)
   let (TArrow
         ( TVar { contents = Unbound ("a", 1) },
           TArrow
             ( TVar { contents = Unbound ("c", 1) },
               TVar { contents = Unbound ("a", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLam
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    PVar (Lexing.dummy_pos, Var_name.of_string "x") ),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))

   (* now sound generalization ! *)
   let (TArrow
         ( TVar
             {
               contents =
                 Link
                   (TArrow
                     ( TVar { contents = Unbound ("b", 1) },
                       TVar { contents = Unbound ("c", 1) },
                       { level_old = 1; level_new = 1 } ));
             },
           TArrow
             ( TVar { contents = Unbound ("b", 1) },
               TVar { contents = Unbound ("c", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLam
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        PVar (Lexing.dummy_pos, Var_name.of_string "z") ) ),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))

   (* now sound generalization ! *)
   let (TArrow
         ( TVar
             {
               contents =
                 Link
                   (TArrow
                     ( TVar { contents = Unbound ("b", 1) },
                       TVar
                         {
                           contents =
                             Link
                               (TArrow
                                 ( TVar { contents = Unbound ("b", 1) },
                                   TVar { contents = Unbound ("d", 1) },
                                   { level_old = 1; level_new = 1 } ));
                         },
                       { level_old = 1; level_new = 1 } ));
             },
           TArrow
             ( TVar { contents = Unbound ("b", 1) },
               TVar { contents = Unbound ("d", 1) },
               { level_old = 1; level_new = 1 } ),
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLam
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                PLet
                  ( Lexing.dummy_pos,
                    Var_name.of_string "x",
                    Some TEInt,
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        PVar (Lexing.dummy_pos, Var_name.of_string "y") ),
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ) ) ))
   ;;

   (* now sound generalization ! *)
   try
     let _ =
       top_type_check
         (PLam
            ( Lexing.dummy_pos,
              Var_name.of_string "x",
              PLet
                ( Lexing.dummy_pos,
                  Var_name.of_string "y",
                  Some TEInt,
                  PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                  PApp
                    ( Lexing.dummy_pos,
                      PVar (Lexing.dummy_pos, Var_name.of_string "y"),
                      PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ) ))
     in
     assert false
   with Failure e -> print_endline e

   (* now sound generalization ! *)
   (* fun x -> let y = let z = x (fun x -> x) in z in y;;
      - : (('a -> 'a) -> 'b) -> 'b = <fun>
   *)
   let (TArrow
         ( TVar
             {
               contents =
                 Link
                   (TArrow
                     ( TArrow
                         ( TVar { contents = Unbound ("b", 1) },
                           TVar { contents = Unbound ("b", 1) },
                           { level_old = 1; level_new = 1 } ),
                       TVar { contents = Unbound ("c", 1) },
                       { level_old = 1; level_new = 1 } ));
             },
           TVar { contents = Unbound ("c", 1) },
           { level_old = 1; level_new = 1 } )) =
     top_type_check
       (PLam
          ( Lexing.dummy_pos,
            Var_name.of_string "x",
            PLet
              ( Lexing.dummy_pos,
                Var_name.of_string "y",
                Some TEInt,
                PLet
                  ( Lexing.dummy_pos,
                    Var_name.of_string "z",
                    Some TEInt,
                    PApp
                      ( Lexing.dummy_pos,
                        PVar (Lexing.dummy_pos, Var_name.of_string "x"),
                        id ),
                    PVar (Lexing.dummy_pos, Var_name.of_string "z") ),
                PVar (Lexing.dummy_pos, Var_name.of_string "y") ) ))
   ;; *)
