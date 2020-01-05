open Stdint

type no_insn
type 'arch mode = int

module Endian = struct
  type big    = [ `BE ]
  type little = [ `LE ]

  type _ t = BE : big t
           | LE : little t

  module type S = sig
    type endian
    val endian : endian t
    val mode   : Uc_const.Mode.t
  end

  module Big = struct
    type endian = big
    let endian  = BE
    let mode    = Uc_const.Mode.big_endian
  end

  module Little = struct
    type endian = little
    let endian  = LE
    let mode    = Uc_const.Mode.little_endian
  end
end

module Arch = struct
  type aarch   = [ `AARCH ]
  type arm     = [ `ARM ]
  type m68k    = [ `M68K ]
  type mips    = [ `MIPS ]
  type mips64  = [ `MIPS64 ]
  type sparc   = [ `SPARC ]
  type sparc64 = [ `SPARC64 ]
  type x86     = [ `X86 ]
  type x86_64  = [ `X64_64 ]

  type all = [ aarch
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
  type arm   = [ Arch.arm | Arch.aarch ]
  type m68k  = Arch.m68k
  type mips  = [ Arch.mips | Arch.mips64 ]
  type sparc = [ Arch.sparc | Arch.sparc64 ]
  type x86   = [ Arch.x86 | Arch.x86_64 ]
end

module Word = struct
  type _ size = W8 : uint8 size
              | W16 : uint16 size
              | W32 : uint32 size
              | W64 : uint64 size

  module type S = sig
    type word
    val word : word size
  end
end

module Mode = struct
  let arm      : [< `ARM ] mode              = (Uc_const.Mode.arm :> int)
  let thumb    : [< `ARM ] mode              = (Uc_const.Mode.thumb :> int)
  let mclass   : [< `ARM ] mode              = (Uc_const.Mode.mclass :> int)
  let v8       : [< `ARM ] mode              = (Uc_const.Mode.mclass :> int)
  let micro    : [< `MIPS | `MIPS64 ] mode   = (Uc_const.Mode.micro :> int)
  let mips3    : [< `MIPS | `MIPS64 ] mode   = (Uc_const.Mode.mips3 :> int)
  let mips32r6 : [< `MIPS ] mode             = (Uc_const.Mode.mips32r6 :> int)
  let v9       : [< `SPARC | `SPARC64 ] mode = (Uc_const.Mode.v9 :> int)

  let (&) : 'a mode -> 'a mode -> 'a mode = (lor)
end

type handle
type ('family, 'endian, 'word) engine = {
  family    : 'family;
  endian    : 'endian Endian.t;
  word_size : 'word Word.size;
  handle    : handle;
}

module type S = sig
  type arch
  type family
  type word
  type endian

  val create : ?mode:arch mode -> unit -> (family, endian, word) engine
end

module type Into16 = sig
  include S
  val into_m16 : (family, endian, word) engine -> (family, endian, uint16) engine
end

module type Into32 = sig
  include S
  val into_m32 : (family, endian, word) engine -> (family, endian, uint32) engine
end

module type Into64 = sig
  include S
  val into_m64 : (family, endian, word) engine -> (family, endian, uint64) engine
end

external create_ffi : arch:Uc_const.Arch.t -> mode:int -> handle = "ml_unicorn_create"

let engine ~family ~endian ~word_size handle = {
  family; endian; word_size; handle
}
