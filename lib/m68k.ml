open Stdint
open Types

type arch   = Arch.m68k
type family = Family.m68k
type word   = uint32

module Reg = struct
  module Id = M68k_const.Reg
  (* TODO *)
end

let create ?mode () =
  let default_mode = Mode.little_endian in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.m68k ~mode in
  engine ~family:`M68K ~endian:Endian.Little ~word_size:Size.W32 h
