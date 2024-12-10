type typc =
  | Tint
  | Tarrow of typ * typ
  | Tproduct of typ * typ
and typ =
  | Te of typc
  | Tvar of tvar
and tvar =
  { id : int;
    mutable def : typ option }