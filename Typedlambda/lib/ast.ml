
type gterm =
  | Lint of int
  | Lbool of bool

               
type typ =
  | Tint   
  | Tbool
  | Tfn of typ * typ (* function type *)


type expr =
  | Var of string
  | Lit of gterm
  | Lam of string * typ * expr
  | App of expr * expr

                    
(* type checker *)

let extend (xt : string * typ) (env : (string * typ) list) =
  xt :: env

let rec lookupVar x env =
  List.assoc x env

exception Typemismatch of string
let rec check exp env =
  match exp with
  | Var x -> lookupVar x env
  | Lit (Lint _) -> Tint
  | Lit (Lbool _) -> Tbool
  | Lam (x, t, e) ->
     let tex = check e ((x, t) :: env) in (* extend the environment *)
     Tfn (t, tex)
  | App (eone, etwo) ->
     let t1 = check eone env in
     let t2 = check etwo env in
     match t1 with
     | Tfn (a, b) ->  if a = t2 then b else (raise (Typemismatch "Type does not match"))
     | _ -> raise (Typemismatch "Not function type")

(* Test cases 
  check (Var "x") [("x", Tint)] => Tint
utop[31]> check (Lam ("x", Tint, Var "x")) [];;
- : typ = Tfn (Tint, Tint)
utop[32]> check (Lam ("x", Tint, Var "y")) [("y", Tint)];;
- : typ = Tfn (Tint, Tint)
utop[33]> check (App (Lam ("x", Tint, Var "x"), Var "y")) [("y", Tint)];;
- : typ = Tint

 *)              
