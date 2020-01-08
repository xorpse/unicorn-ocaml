open Stdint

module Arm     : module type of Arm
module Aarch64 : module type of Aarch64
module M68k    : module type of M68k
module Mips64  : module type of Mips.M64
module Mips    : module type of Mips.M32
module Sparc64 : module type of Sparc.M32
module Sparc   : module type of Sparc.M32
module X86     : module type of X86

module Types   : sig
module Arch : module type of Types.Arch
module Endian : module type of Types.Endian
module Family : module type of Types.Family
module Size : module type of Types.Size

module Mode : sig
  type 'arch t = 'arch Types.Mode.t

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
type ('family, 'word) engine = ('family, 'word) Types.engine

module type S = Types.S
module type S_Reg = Types.S_Reg

type (+'arch, +'id, 'word) reg
end

module Arch    : module type of Types.Arch
module Endian  : module type of Types.Endian
module Family  : module type of Types.Family
module Mode    : module type of Types.Mode

module Const   : sig
  module Err   : sig
    type t

    val arch            : t
    val arg             : t
    val exception_      : t
    val fetch_prot      : t
    val fetch_unaligned : t
    val fetch_unmapped  : t
    val handle          : t
    val hook            : t
    val hook_exist      : t
    val map             : t
    val mode            : t
    val nomem           : t
    val read_prot       : t
    val read_unaligned  : t
    val read_unmapped   : t
    val resource        : t
    val version         : t
    val write_prot      : t
    val write_unaligned : t
    val write_unmapped  : t
  end
end

open! Types

module X86_64  : sig
  type arch   = Arch.x86_64
  type family = Family.x86
  type word   = uint64

  module Insn : module type of X86_64.Insn
  module Reg : sig
    module Id : module type of X86_64.Reg.Id

    val al : (arch, Id.t, uint8) reg
  end

  val create : ?mode : arch Mode.t -> unit -> (family, word) engine
end

exception Unicorn_error of Const.Err.t

module Register : sig
  val write : (module S_Reg with type arch = 'a
                             and type family = 'f
                             and type word = 'w
                             and type Reg.Id.t = 'r)
    -> ('f, 'w) engine
    -> ('a, 'r, 's) reg
    -> 's
    -> unit

  val read : (module S_Reg with type arch = 'a
                            and type family = 'f
                            and type word = 'w
                            and type Reg.Id.t = 'r)
    -> ('f, 'w) engine
    -> ('a, 'r, 's) reg
    -> 's
end

module Memory : sig
  val read_word : (module S with type family = 'f
                             and type word = 'w)
    -> ('f, 'w) engine
    -> 'w
    -> 'w

  val write_word : (module S with type family = 'f
                              and type word = 'w)
    -> ('f, 'w) engine
    -> 'w
    -> 'w
    -> unit
end

val create : ?mode:'a Mode.t
  -> (module S with type arch = 'a
                and type family = 'f
                and type word = 'w)
  -> ('f, 'w) engine
val version : unit -> int * int
