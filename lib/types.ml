open Stdint

module Arch = struct
  type aarch64 = [ `AARCH64 ]
  type arm     = [ `ARM ]
  type m68k    = [ `M68K ]
  type mips    = [ `MIPS ]
  type mips64  = [ `MIPS64 ]
  type sparc   = [ `SPARC ]
  type sparc64 = [ `SPARC64 ]
  type x86     = [ `X86 ]
  type x86_64  = [ `X86_64 ]

  type all = [ aarch64
             | arm
             | m68k
             | mips
             | mips64
             | sparc
             | sparc64
             | x86
             | x86_64
             ]
  type any = all
end

module Family = struct
  type arm   = [ Arch.arm | Arch.aarch64 ]
  type m68k  = Arch.m68k
  type mips  = [ Arch.mips | Arch.mips64 ]
  type sparc = [ Arch.sparc | Arch.sparc64 ]
  type x86   = [ Arch.x86 | Arch.x86_64 ]

  type all = [ arm | m68k | mips | sparc | x86 ]
  type any = all
end

module Size = struct
  type _ t = W8  : uint8 t
           | W16 : uint16 t
           | W32 : uint32 t
           | W64 : uint64 t

  module type S = sig
    type word
    val word : word t
  end
end

module Mode = struct
  type 'arch t = int

  let arm      : [< Arch.arm ] t     = (Uc_const.Mode.arm :> int)
  let thumb    : [< Arch.arm ] t     = (Uc_const.Mode.thumb :> int)
  let mclass   : [< Arch.arm ] t     = (Uc_const.Mode.mclass :> int)
  let v8       : [< Arch.arm ] t     = (Uc_const.Mode.mclass :> int)
  let micro    : [< Family.mips ] t  = (Uc_const.Mode.micro :> int)
  let mips3    : [< Family.mips ] t  = (Uc_const.Mode.mips3 :> int)
  let mips32r6 : [< Arch.mips ] t    = (Uc_const.Mode.mips32r6 :> int)
  let v9       : [< Family.sparc ] t = (Uc_const.Mode.v9 :> int)

  let mode_32  : [< Arch.arm | Arch.m68k | Arch.mips | Arch.sparc | Arch.x86 ] t = (Uc_const.Mode.mode_32 :> int)
  let mode_64  : [< Arch.aarch64 | Arch.mips64 | Arch.sparc64 | Arch.x86_64 ] t  = (Uc_const.Mode.mode_64 :> int)

  let little_endian : [< Family.x86 | Family.arm | Family.mips | Family.m68k ] t = (Uc_const.Mode.little_endian :> int)
  let big_endian    : [< Family.arm | Family.mips | Family.sparc ] t             = (Uc_const.Mode.big_endian :> int)

  let (&) : 'a t -> 'a t -> 'a t = (lor)
end

module Endian = struct
  type t = Big | Little

  module type S = sig
    val endian : t
  end

  module Big : S = struct
    let endian = Big
  end

  module Little : S = struct
    let endian = Little
  end
end


type handle
type ('family, 'word) engine = {
  family    : 'family;
  endian    : Endian.t;
  word_size : 'word Size.t;
  handle    : handle;
}

module type S = sig
  type arch
  type family
  type word

  val create : ?mode:arch Mode.t -> unit -> (family, word) engine
end

module type S_Reg = sig
  include S
  module Reg : sig
    module Id : sig
      type t
    end
  end
end

type ('arch, 'id, 'word) reg = 'arch * 'id * 'word Size.t

external create_ffi : arch:Uc_const.Arch.t -> mode:'arch Mode.t -> handle = "ml_unicorn_create"

let engine ~family ~endian ~word_size handle = {
  family; endian; word_size; handle
}
