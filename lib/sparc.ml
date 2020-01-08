open Stdint
open Types

module Make (W : Size.S) = struct
  type family = Family.sparc
  type word = W.word

  module Reg = struct
    (* word dep. size *)
    module Id = Sparc_const.Reg
  end
end

module M32 = struct
  type arch = Arch.sparc

  include Make (struct type word = uint32 let word = Size.W32 end)
end

module M64 = struct
  type arch = Arch.sparc64

  include Make (struct type word = uint64 let word = Size.W64 end)
end
