open Stdint
open Types

type arch   = Arch.aarch
type family = Family.arm
type word   = uint64

module Reg = struct
  (* TODO *)
end

module Make (E : Endian.S) = struct
  type nonrec arch   = arch
  type nonrec family = family
  type nonrec word   = word
  type nonrec endian = E.endian

  let create ?mode () =
    let default_mode = (E.mode :> arch mode) in
    let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
    let h = create_ffi ~arch:Uc_const.Arch.arm64 ~mode in
    engine ~family:`ARM ~endian:E.endian ~word_size:Word.W64 h
end
