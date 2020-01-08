open Stdint
open Types

type arch   = Arch.arm
type family = Family.arm
type word   = uint32

module Reg = struct
  module Id = Arm_const.Reg
  (* TODO *)
end

let create ?mode ?endian () =
  let endian = match endian with Some e -> e | None -> Endian.Little in
  let default_mode = match endian with
    | Endian.Big -> Mode.big_endian
    | Endian.Little -> Mode.little_endian
  in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.arm ~mode in
  engine ~family:`ARM ~endian:endian ~word_size:Size.W32 h
