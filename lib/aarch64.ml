open Stdint
open Types

type arch   = Arch.aarch64
type family = Family.arm
type word   = uint64

(* Aarch32 exists; is it possible in Unicorn to switch to
   this mode via MODE_32 option in the setup?

   The following issue seems to imply it's not possible:
   https://github.com/unicorn-engine/unicorn/issues/1070

   On the master branch (https://github.com/unicorn-engine/unicorn/blob/8621bca53758532ad6a4ec5d17684fcdb9923cc6/uc.c#L208)
   it seems to be prohibited when creating a handle.
*)

module Reg = struct
  module Id = Arm64_const.Reg
  (* TODO *)
end

let create ?mode ?endian () =
  let endian = match endian with Some e -> e | None -> Endian.Little in
  let default_mode = match endian with
    | Endian.Big -> Mode.big_endian
    | Endian.Little -> Mode.little_endian
  in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.arm64 ~mode in
  engine ~family:`ARM ~endian:endian ~word_size:Size.W64 h
