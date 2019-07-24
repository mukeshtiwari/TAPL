
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

                    
