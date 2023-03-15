open Sexplib.Std
   
type 'r rel_imm =
  | RAp of 'r
  | RConst of int [@@deriving sexp]

type 'r rel_op =
    Nu
  | RImm of 'r rel_imm [@@deriving sexp]

type 'r refinement_rel = {
  rel_op1: 'r rel_op;
  rel_cond: string;
  rel_op2: 'r rel_imm;
  } [@@deriving sexp]
                       
type ('c,'r) refinement =
  | Pred of string * 'c
  | CtxtPred of int * string * 'c
  | Top
  | ConstEq of int
  | Relation of 'r refinement_rel
  | And of ('c,'r) refinement * ('c,'r) refinement
  | NamedPred of string * 'c [@@deriving sexp]

type refine_ap = [
  Paths.concr_ap
  | `Sym of int
  ] [@@deriving sexp]

type ap_symb =
  | SVar of string
  | SProj of int [@@deriving sexp]

           
type ty_binding = (int * ap_symb) list [@@deriving sexp]
type rec_args = (int * int) list [@@deriving sexp]
type arr_bind = {len: int; ind: int} [@@deriving sexp]
              
type ('a,'o,'m,'n) _typ =
  | Int of 'a
  | Ref of ('a,'o,'m,'n) _typ * 'o * 'n
  | Tuple of ty_binding * (('a,'o,'m,'n) _typ) list
  | TVar of int
  | Mu of rec_args * 'm * int * ('a, 'o,'m,'n) _typ
  | Array of arr_bind * 'a * 'o * ('a,'o,'m,'n) _typ
[@@deriving sexp]

type nullity =
  | NUnk
  | NNull
  | NLive [@@deriving sexp]

type mu_ap =
    MRoot
  | MProj of mu_ap * int
  | MDeref of mu_ap
  | MElem of mu_ap
  | MLen of mu_ap [@@deriving sexp]
          
type 'a inductive_preds = {
    pred_symbols: (mu_ap * 'a) list;
    fv_map: (mu_ap * (int list * int list)) list;
  } [@@deriving sexp]
                        
type symbolic_refinement = (refine_ap list,refine_ap) refinement [@@deriving sexp]
type typ = (symbolic_refinement,bool,symbolic_refinement inductive_preds,nullity) _typ [@@deriving sexp]
type src_typ = (symbolic_refinement,float,symbolic_refinement inductive_preds,nullity) _typ [@@deriving sexp]

