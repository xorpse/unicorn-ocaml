open Stdint

module Arm     : module type of Arm
module Aarch64 : module type of Aarch64
module M68k    : module type of M68k
module Mips64  : module type of Mips.M64
module Mips    : module type of Mips.M32
module Sparc64 : module type of Sparc.M32
module Sparc   : module type of Sparc.M32

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
  type ('family, 'word) engine
  type (+'arch, +'id, 'word) reg

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
      val read  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
      val write : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
    end
  end
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

module X86     : sig
  type arch   = Arch.x86
  type family = Family.x86
  type word   = uint32

  module Insn : module type of X86.Insn
  module Reg : sig
    module Id : module type of X86.Reg.Id

    val al     : (arch, Id.t, uint8) reg
    val ax     : (arch, Id.t, uint16) reg
    val eax    : (arch, Id.t, uint32) reg
    val ah     : (arch, Id.t, uint8) reg
    val cl     : (arch, Id.t, uint8) reg
    val cx     : (arch, Id.t, uint16) reg
    val ecx    : (arch, Id.t, uint32) reg
    val ch     : (arch, Id.t, uint8) reg
    val dl     : (arch, Id.t, uint8) reg
    val dx     : (arch, Id.t, uint16) reg
    val edx    : (arch, Id.t, uint32) reg
    val dh     : (arch, Id.t, uint8) reg
    val bl     : (arch, Id.t, uint8) reg
    val bx     : (arch, Id.t, uint16) reg
    val ebx    : (arch, Id.t, uint32) reg
    val bh     : (arch, Id.t, uint8) reg
    val spl    : (arch, Id.t, uint8) reg
    val sp     : (arch, Id.t, uint16) reg
    val esp    : (arch, Id.t, uint32) reg
    val bpl    : (arch, Id.t, uint8) reg
    val bp     : (arch, Id.t, uint16) reg
    val ebp    : (arch, Id.t, uint32) reg
    val sil    : (arch, Id.t, uint8) reg
    val si     : (arch, Id.t, uint16) reg
    val esi    : (arch, Id.t, uint32) reg
    val dil    : (arch, Id.t, uint8) reg
    val di     : (arch, Id.t, uint16) reg
    val edi    : (arch, Id.t, uint32) reg
    val es     : (arch, Id.t, uint16) reg
    val cs     : (arch, Id.t, uint16) reg
    val ss     : (arch, Id.t, uint16) reg
    val ds     : (arch, Id.t, uint16) reg
    val fs     : (arch, Id.t, uint16) reg
    val gs     : (arch, Id.t, uint16) reg
    val flags  : (arch, Id.t, uint16) reg
    val eflags : (arch, Id.t, uint32) reg
    val ip     : (arch, Id.t, uint16) reg
    val eip    : (arch, Id.t, uint32) reg
    val eiz    : (arch, Id.t, uint32) reg
    val dr0    : (arch, Id.t, uint32) reg
    val dr1    : (arch, Id.t, uint32) reg
    val dr2    : (arch, Id.t, uint32) reg
    val dr3    : (arch, Id.t, uint32) reg
    val dr4    : (arch, Id.t, uint32) reg
    val dr5    : (arch, Id.t, uint32) reg
    val dr6    : (arch, Id.t, uint32) reg
    val dr7    : (arch, Id.t, uint32) reg
    val cr0    : (arch, Id.t, uint32) reg
    val cr1    : (arch, Id.t, uint32) reg
    val cr2    : (arch, Id.t, uint32) reg
    val cr3    : (arch, Id.t, uint32) reg
    val cr4    : (arch, Id.t, uint32) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> unit -> (family, word) engine
end

module X86_64  : sig
  type arch   = Arch.x86_64
  type family = Family.x86
  type word   = uint64

  module Insn : module type of X86_64.Insn
  module Reg : sig
    module Id : module type of X86_64.Reg.Id

    val al     : (arch, Id.t, uint8) reg
    val ax     : (arch, Id.t, uint16) reg
    val eax    : (arch, Id.t, uint32) reg
    val rax    : (arch, Id.t, uint64) reg
    val ah     : (arch, Id.t, uint8) reg
    val cl     : (arch, Id.t, uint8) reg
    val cx     : (arch, Id.t, uint16) reg
    val ecx    : (arch, Id.t, uint32) reg
    val rcx    : (arch, Id.t, uint64) reg
    val ch     : (arch, Id.t, uint8) reg
    val dl     : (arch, Id.t, uint8) reg
    val dx     : (arch, Id.t, uint16) reg
    val edx    : (arch, Id.t, uint32) reg
    val rdx    : (arch, Id.t, uint64) reg
    val dh     : (arch, Id.t, uint8) reg
    val bl     : (arch, Id.t, uint8) reg
    val bx     : (arch, Id.t, uint16) reg
    val ebx    : (arch, Id.t, uint32) reg
    val rbx    : (arch, Id.t, uint64) reg
    val bh     : (arch, Id.t, uint8) reg
    val spl    : (arch, Id.t, uint8) reg
    val sp     : (arch, Id.t, uint16) reg
    val esp    : (arch, Id.t, uint32) reg
    val rsp    : (arch, Id.t, uint64) reg
    val bpl    : (arch, Id.t, uint8) reg
    val bp     : (arch, Id.t, uint16) reg
    val ebp    : (arch, Id.t, uint32) reg
    val rbp    : (arch, Id.t, uint64) reg
    val sil    : (arch, Id.t, uint8) reg
    val si     : (arch, Id.t, uint16) reg
    val esi    : (arch, Id.t, uint32) reg
    val rsi    : (arch, Id.t, uint64) reg
    val dil    : (arch, Id.t, uint8) reg
    val di     : (arch, Id.t, uint16) reg
    val edi    : (arch, Id.t, uint32) reg
    val rdi    : (arch, Id.t, uint64) reg
    val r8b    : (arch, Id.t, uint8) reg
    val r8w    : (arch, Id.t, uint16) reg
    val r8d    : (arch, Id.t, uint32) reg
    val r8     : (arch, Id.t, uint64) reg
    val r9b    : (arch, Id.t, uint8) reg
    val r9w    : (arch, Id.t, uint16) reg
    val r9d    : (arch, Id.t, uint32) reg
    val r9     : (arch, Id.t, uint64) reg
    val r10b   : (arch, Id.t, uint8) reg
    val r10w   : (arch, Id.t, uint16) reg
    val r10d   : (arch, Id.t, uint32) reg
    val r10    : (arch, Id.t, uint64) reg
    val r11b   : (arch, Id.t, uint8) reg
    val r11w   : (arch, Id.t, uint16) reg
    val r11d   : (arch, Id.t, uint32) reg
    val r11    : (arch, Id.t, uint64) reg
    val r12b   : (arch, Id.t, uint8) reg
    val r12w   : (arch, Id.t, uint16) reg
    val r12d   : (arch, Id.t, uint32) reg
    val r12    : (arch, Id.t, uint64) reg
    val r13b   : (arch, Id.t, uint8) reg
    val r13w   : (arch, Id.t, uint16) reg
    val r13d   : (arch, Id.t, uint32) reg
    val r13    : (arch, Id.t, uint64) reg
    val r14b   : (arch, Id.t, uint8) reg
    val r14w   : (arch, Id.t, uint16) reg
    val r14d   : (arch, Id.t, uint32) reg
    val r14    : (arch, Id.t, uint64) reg
    val r15b   : (arch, Id.t, uint8) reg
    val r15w   : (arch, Id.t, uint16) reg
    val r15d   : (arch, Id.t, uint32) reg
    val r15    : (arch, Id.t, uint64) reg
    val es     : (arch, Id.t, uint16) reg
    val cs     : (arch, Id.t, uint16) reg
    val ss     : (arch, Id.t, uint16) reg
    val ds     : (arch, Id.t, uint16) reg
    val fs     : (arch, Id.t, uint16) reg
    val gs     : (arch, Id.t, uint16) reg
    val flags  : (arch, Id.t, uint16) reg
    val eflags : (arch, Id.t, uint32) reg
    val rflags : (arch, Id.t, uint64) reg
    val ip     : (arch, Id.t, uint16) reg
    val eip    : (arch, Id.t, uint32) reg
    val eiz    : (arch, Id.t, uint32) reg
    val rip    : (arch, Id.t, uint64) reg
    val riz    : (arch, Id.t, uint64) reg
    val dr0    : (arch, Id.t, uint64) reg
    val dr1    : (arch, Id.t, uint64) reg
    val dr2    : (arch, Id.t, uint64) reg
    val dr3    : (arch, Id.t, uint64) reg
    val dr4    : (arch, Id.t, uint64) reg
    val dr5    : (arch, Id.t, uint64) reg
    val dr6    : (arch, Id.t, uint64) reg
    val dr7    : (arch, Id.t, uint64) reg
    val cr0    : (arch, Id.t, uint64) reg
    val cr1    : (arch, Id.t, uint64) reg
    val cr2    : (arch, Id.t, uint64) reg
    val cr3    : (arch, Id.t, uint64) reg
    val cr4    : (arch, Id.t, uint64) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
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
  module Permission : sig
    type t

    val read    : t
    val write   : t
    val execute : t
    val all     : t
    val none    : t

    val (&) : t -> t -> t
  end

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

  val read_bytes  : ('f, 'w) engine -> 'w -> int -> bytes
  val write_bytes : ('f, 'w) engine -> 'w -> bytes -> unit

  val map       : ('f, 'w) engine -> 'w -> int -> Permission.t -> unit
  val map_bytes : ('f, 'w) engine -> 'w -> bytes -> Permission.t -> unit
  val unmap     : ('f, 'w) engine -> 'w -> int -> unit

  val protect   : ('f, 'w) engine -> 'w -> int -> Permission.t -> unit
end

module Hook : sig
  module Memory : sig
    module Access : sig
      module Valid : sig
        type t

        val read : t
        val write : t
        val fetch : t
        val read_after : t

        val (&) : t -> t -> t
      end

      module Invalid : sig
        type t

        val read_unmapped : t
        val write_unmapped : t
        val fetch_unmapped : t

        val read_prot : t
        val write_prot : t
        val fetch_prot : t

        val (&) : t -> t -> t
      end
    end
  end

  type 'v cont = Continue of 'v
               | Stop of 'v

  type ('f, 'w, 'v) callback
  type ('f, 'w) handle

  val code   : (('f, 'w) engine -> int64 -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
  val intr   : (('f, 'w) engine -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
  val mem    : (('f, 'w) engine -> Memory.Access.Valid.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> Memory.Access.Valid.t -> ('f, 'w, 'v) callback
  val mem_ev : (('f, 'w) engine -> Memory.Access.Invalid.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> Memory.Access.Invalid.t -> ('f, 'w, 'v) callback

  val add    : ('f, 'w) engine -> ('f, 'w, 'v) callback -> 'v -> ('f, 'w) handle
  val remove : ('f, 'w) engine -> ('f, 'w) handle -> unit
end

type timeout

val no_timeout : timeout
val timeout    : uint64 -> timeout

type instruction_count

val no_limit : instruction_count
val limit    : int -> instruction_count

val start   : ?timeout:timeout -> ?limit:instruction_count -> address:'w -> until:'w -> ('f, 'w) engine -> unit
val stop    : ('f, 'w) engine -> unit
val version : unit -> int * int
