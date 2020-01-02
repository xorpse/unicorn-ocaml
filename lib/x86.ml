module Const = X86_const

type mmr = {
  selector : int option;
  base     : int64;
  limit    : int32;
  flags    : int32 option;
}

type msr = {
  rid   : int32;
  value : int64;
}
