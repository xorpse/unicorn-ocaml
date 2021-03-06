open Stdint

module Arch : sig
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

module Endian : sig
  type t = Big | Little

  module type S = sig
    val endian : t
  end

  module Big : S
  module Little : S
end

module Family : sig
  type arm   = [ Arch.arm | Arch.aarch64 ]
  type m68k  = Arch.m68k
  type mips  = [ Arch.mips | Arch.mips64 ]
  type sparc = [ Arch.sparc | Arch.sparc64 ]
  type x86   = [ Arch.x86 | Arch.x86_64 ]

  type all = [ arm | m68k | mips | sparc | x86 ]
  type any = all
end

module Size : sig
  type _ t = W8  : uint8 t
           | W16 : uint16 t
           | W32 : uint32 t
           | W64 : uint64 t

  module type S = sig
    type word
    val word : word t
  end
end

module Mode : sig
  type 'arch t = int

  val arm      : [< Arch.arm ] t
  val thumb    : [< Arch.arm ] t
  val mclass   : [< Arch.arm ] t
  val v8       : [< Arch.arm ] t
  val micro    : [< Family.mips ] t
  val mips3    : [< Family.mips ] t
  val mips32r6 : [< Arch.mips ] t
  val v9       : [< Family.sparc ] t

  val mode_32  : [< Arch.arm | Arch.m68k | Arch.mips | Arch.sparc | Arch.x86 ] t
  val mode_64  : [< Arch.aarch64 | Arch.mips64 | Arch.sparc64 | Arch.x86_64 ] t

  val little_endian : [< Family.x86 | Family.arm | Family.mips | Family.m68k ] t
  val big_endian    : [< Family.arm | Family.mips | Family.sparc ] t

  val (&) : 'a t -> 'a t -> 'a t
end

type handle
type ('family, 'word) engine

module type S = sig
  type arch
  type family
  type word

  val create : ?mode:arch Mode.t -> unit -> (family, word) engine
end

type ('arch, 'id, 'word) reg = 'arch * 'id * 'word Size.t

module type S_Reg = sig
  include S
  module Reg : sig
    module Id : sig
      type t
    end
    val read  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end
end

val reg_write_uint8_ffi : handle -> int -> uint8 -> unit
val reg_write_uint16_ffi : handle -> int -> uint16 -> unit
val reg_write_uint32_ffi : handle -> int -> uint32 -> unit
val reg_write_uint64_ffi : handle -> int -> uint64 -> unit

val reg_read_uint8_ffi : handle -> int -> uint8
val reg_read_uint16_ffi : handle -> int -> uint16
val reg_read_uint32_ffi : handle -> int -> uint32
val reg_read_uint64_ffi : handle -> int -> uint64

val create_ffi : arch:Uc_const.Arch.t -> mode:'arch Mode.t -> handle

val engine : family:'family -> endian:Endian.t -> word_size:'w Size.t -> handle -> ('family, 'w) engine
val family : ('f, 'w) engine -> 'f
val endian : ('f, 'w) engine -> Endian.t
val word_size : ('f, 'w) engine -> 'w Size.t
val handle : ('f, 'w) engine -> handle
