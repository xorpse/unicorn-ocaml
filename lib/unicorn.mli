module Arm   : module type of Arm
module Arm64 : module type of Arm64
module M68k  : module type of M68k
module Mips  : module type of Mips
module Sparc : module type of Sparc
module X86   : module type of X86

module Const : sig
  module Err : module type of Uc_const.Err
end

exception Unicorn_error of Const.Err.t

module Arch : sig
  type id  = [ `ARM
             | `ARM64
             | `M68K
             | `MIPS
             | `SPARC
             | `X86
             ]
  type any = id
  type no_insn
  type ('a, 'r, 'i) t =
    | ARM   : ([ `ARM ], Arm.Const.Reg.t, no_insn) t
    | ARM64 : ([ `ARM64 ], Arm64.Const.Reg.t, no_insn) t
    | M68K  : ([ `M68K ], M68k.Const.Reg.t, no_insn) t
    | MIPS  : ([ `MIPS ], Mips.Const.Reg.t, no_insn) t
    | SPARC : ([ `SPARC ], Sparc.Const.Reg.t, no_insn) t
    | X86   : ([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) t
end

module Mode : sig
  type 'arch t

  val little_endian : [< Arch.any ] t
  val arm           : [< `ARM ] t
  val mode_16       : [< `X86 ] t
  val mode_32       : [< `MIPS | `SPARC | `X86 ] t
  val mode_64       : [< `MIPS | `SPARC | `X86 ] t
  val thumb         : [< `ARM ] t
  val mclass        : [< `ARM ] t
  val v8            : [< `ARM ] t
  val micro         : [< `MIPS ] t
  val mips3         : [< `MIPS ] t
  val mips32r6      : [< `MIPS ] t
  val big_endian    : [< Arch.any ] t
  val mips32        : [< `MIPS ] t
  val mips64        : [< `MIPS ] t
  val sparc32       : [< `SPARC ] t
  val sparc64       : [< `SPARC ] t
  val v9            : [< `SPARC ] t
  val (&)           : 'arch t -> 'arch t -> 'arch t
end

type ('a, 'r, 'i) engine

val create  : ?mode:'a Mode.t -> ('a, 'r, 'i) Arch.t -> ('a, 'r, 'i) engine
val version : unit -> int * int

module Memory : sig
  module Access : sig
    type t
  end

  module Permission : sig
    type t
  end
end

module Hook : sig
  module Mem : sig
    module Valid : sig
      type t
      val read       : t
      val write      : t
      val fetch      : t
      val read_after : t
      val (&)        : t -> t -> t
    end
    module Invalid : sig
      type t
      val read_unmapped  : t
      val write_unmapped : t
      val fetch_unmapped : t
      val read_prot      : t
      val write_prot     : t
      val fetch_prot     : t
      val (&)            : t -> t -> t
    end
  end

  type 'v cont = Continue of 'v
               | Stop of 'v

  type ('a, 'r, 'i) handle
  type ('a, 'r, 'i, 'v) callback

  val code   : (('a, 'r, 'i) engine -> int64 -> int32 -> 'v -> 'v) -> ('a, 'r, 'i, 'v) callback
  val intr   : (('a, 'r, 'i) engine -> int32 -> 'v -> 'v) -> ('a, 'r, 'i, 'v) callback
  val mem    : (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> Mem.Valid.t -> ('a, 'r, 'i, 'v) callback
  val mem_ev : (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> Mem.Invalid.t -> ('a, 'r, 'i, 'v) callback
  val in_    : (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> 'v -> int32 * 'v) -> ([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback
  val out    : (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> int32 -> 'v -> 'v) -> ([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback

  val add : ('a, 'r, 'i) engine -> ('a, 'r, 'i, 'v) callback -> 'v -> ('a, 'r, 'i) handle
  val remove : ('a, 'r, 'i) engine -> ('a, 'r, 'i) handle -> unit
end
