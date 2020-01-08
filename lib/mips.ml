open Stdint
open Types

module Make (W : Size.S) = struct
  type family = Family.mips
  type word = W.word

  module Reg = struct
    module Id = Mips_const.Reg
  end
end

module M32 = struct
  type arch = Arch.mips

  include Make (struct type word = uint32 let word = Size.W32 end)
end

module M64 = struct
  type arch = Arch.mips64

  include Make (struct type word = uint64 let word = Size.W64 end)
end
