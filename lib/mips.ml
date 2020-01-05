open Stdint
open Types

module Make (W : Word.S) (E : Endian.S) = struct
  type family = Family.mips
  type word = W.word
  type endian = E.endian

  module Reg = struct
    (* word dep. size *)
  end
end

module M32 (E: Endian.S) = struct
  type arch = Arch.mips

  include Make (struct type word = uint32 let word = Word.W32 end) (E)
end

module M64 (E : Endian.S) = struct
  type arch = Arch.mips64

  include Make (struct type word = uint64 let word = Word.W64 end) (E)
end
