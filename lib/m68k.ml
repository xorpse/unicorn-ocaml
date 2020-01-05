open Stdint
open Types

type arch   = Arch.m68k
type family = Family.m68k
type word   = uint32
type endian = Endian.little

module Reg = struct
  (* TODO *)
end

let create ?mode () =
  let default_mode = (Endian.Little.mode :> arch mode) in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.m68k ~mode in
  engine ~family:`M68K ~endian:Endian.Little.endian ~word_size:Word.W32 h
