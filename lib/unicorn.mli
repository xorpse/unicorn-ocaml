open Stdint

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

module Arm     : sig
  type arch   = Arch.arm
  type family = Family.arm
  type word   = uint32

  module Reg : sig
    module Id : module type of Arm.Reg.Id

    val r0 : (arch, Id.t, uint32) reg
    val r1 : (arch, Id.t, uint32) reg
    val r2 : (arch, Id.t, uint32) reg
    val r3 : (arch, Id.t, uint32) reg
    val r4 : (arch, Id.t, uint32) reg
    val r5 : (arch, Id.t, uint32) reg
    val r6 : (arch, Id.t, uint32) reg
    val r7 : (arch, Id.t, uint32) reg
    val r8 : (arch, Id.t, uint32) reg
    val r9 : (arch, Id.t, uint32) reg
    val sb : (arch, Id.t, uint32) reg
    val r10 : (arch, Id.t, uint32) reg
    val sl : (arch, Id.t, uint32) reg
    val r11 : (arch, Id.t, uint32) reg
    val fp : (arch, Id.t, uint32) reg
    val r12 : (arch, Id.t, uint32) reg
    val ip : (arch, Id.t, uint32) reg
    val sp : (arch, Id.t, uint32) reg
    val lr : (arch, Id.t, uint32) reg
    val pc : (arch, Id.t, uint32) reg
    val c1_c0_2 : (arch, Id.t, uint32) reg
    val c13_c0_3 : (arch, Id.t, uint32) reg
    val apsr : (arch, Id.t, uint32) reg
    val cpsr : (arch, Id.t, uint32) reg
    val spsr : (arch, Id.t, uint32) reg
    val fpsid : (arch, Id.t, uint32) reg
    val fpscr : (arch, Id.t, uint32) reg
    val fpexc : (arch, Id.t, uint32) reg
    val mvfr0 : (arch, Id.t, uint32) reg
    val mvfr1 : (arch, Id.t, uint32) reg
    val s0 : (arch, Id.t, uint32) reg
    val d0 : (arch, Id.t, uint64) reg
    val s1 : (arch, Id.t, uint32) reg
    val s2 : (arch, Id.t, uint32) reg
    val d1 : (arch, Id.t, uint64) reg
    val s3 : (arch, Id.t, uint32) reg
    val s4 : (arch, Id.t, uint32) reg
    val d2 : (arch, Id.t, uint64) reg
    val s5 : (arch, Id.t, uint32) reg
    val s6 : (arch, Id.t, uint32) reg
    val d3 : (arch, Id.t, uint64) reg
    val s7 : (arch, Id.t, uint32) reg
    val s8 : (arch, Id.t, uint32) reg
    val d4 : (arch, Id.t, uint64) reg
    val s9 : (arch, Id.t, uint32) reg
    val s10 : (arch, Id.t, uint32) reg
    val d5 : (arch, Id.t, uint64) reg
    val s11 : (arch, Id.t, uint32) reg
    val s12 : (arch, Id.t, uint32) reg
    val d6 : (arch, Id.t, uint64) reg
    val s13 : (arch, Id.t, uint32) reg
    val s14 : (arch, Id.t, uint32) reg
    val d7 : (arch, Id.t, uint64) reg
    val s15 : (arch, Id.t, uint32) reg
    val s16 : (arch, Id.t, uint32) reg
    val d8 : (arch, Id.t, uint64) reg
    val s17 : (arch, Id.t, uint32) reg
    val s18 : (arch, Id.t, uint32) reg
    val d9 : (arch, Id.t, uint64) reg
    val s19 : (arch, Id.t, uint32) reg
    val s20 : (arch, Id.t, uint32) reg
    val d10 : (arch, Id.t, uint64) reg
    val s21 : (arch, Id.t, uint32) reg
    val s22 : (arch, Id.t, uint32) reg
    val d11 : (arch, Id.t, uint64) reg
    val s23 : (arch, Id.t, uint32) reg
    val s24 : (arch, Id.t, uint32) reg
    val d12 : (arch, Id.t, uint64) reg
    val s25 : (arch, Id.t, uint32) reg
    val s26 : (arch, Id.t, uint32) reg
    val d13 : (arch, Id.t, uint64) reg
    val s27 : (arch, Id.t, uint32) reg
    val s28 : (arch, Id.t, uint32) reg
    val d14 : (arch, Id.t, uint64) reg
    val s29 : (arch, Id.t, uint32) reg
    val s30 : (arch, Id.t, uint32) reg
    val d15 : (arch, Id.t, uint64) reg
    val s31 : (arch, Id.t, uint32) reg
    val d16 : (arch, Id.t, uint64) reg
    val d17 : (arch, Id.t, uint64) reg
    val d18 : (arch, Id.t, uint64) reg
    val d19 : (arch, Id.t, uint64) reg
    val d20 : (arch, Id.t, uint64) reg
    val d21 : (arch, Id.t, uint64) reg
    val d22 : (arch, Id.t, uint64) reg
    val d23 : (arch, Id.t, uint64) reg
    val d24 : (arch, Id.t, uint64) reg
    val d25 : (arch, Id.t, uint64) reg
    val d26 : (arch, Id.t, uint64) reg
    val d27 : (arch, Id.t, uint64) reg
    val d28 : (arch, Id.t, uint64) reg
    val d29 : (arch, Id.t, uint64) reg
    val d30 : (arch, Id.t, uint64) reg
    val d31 : (arch, Id.t, uint64) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> ?endian : Endian.t -> unit -> (family, word) engine
end

module Aarch64 : sig
  type arch   = Arch.aarch64
  type family = Family.arm
  type word   = uint64

  module Reg : sig
    module Id : module type of Aarch64.Reg.Id

    val x0 : (arch, Id.t, uint64) reg
    val x1 : (arch, Id.t, uint64) reg
    val x2 : (arch, Id.t, uint64) reg
    val x3 : (arch, Id.t, uint64) reg
    val x4 : (arch, Id.t, uint64) reg
    val x5 : (arch, Id.t, uint64) reg
    val x6 : (arch, Id.t, uint64) reg
    val x7 : (arch, Id.t, uint64) reg
    val x8 : (arch, Id.t, uint64) reg
    val x9 : (arch, Id.t, uint64) reg
    val x10 : (arch, Id.t, uint64) reg
    val x11 : (arch, Id.t, uint64) reg
    val x12 : (arch, Id.t, uint64) reg
    val x13 : (arch, Id.t, uint64) reg
    val x14 : (arch, Id.t, uint64) reg
    val x15 : (arch, Id.t, uint64) reg
    val x16 : (arch, Id.t, uint64) reg
    val x17 : (arch, Id.t, uint64) reg
    val x18 : (arch, Id.t, uint64) reg
    val x19 : (arch, Id.t, uint64) reg
    val x20 : (arch, Id.t, uint64) reg
    val x21 : (arch, Id.t, uint64) reg
    val x22 : (arch, Id.t, uint64) reg
    val x23 : (arch, Id.t, uint64) reg
    val x24 : (arch, Id.t, uint64) reg
    val x25 : (arch, Id.t, uint64) reg
    val x26 : (arch, Id.t, uint64) reg
    val x27 : (arch, Id.t, uint64) reg
    val x28 : (arch, Id.t, uint64) reg
    val x29 : (arch, Id.t, uint64) reg
    val x30 : (arch, Id.t, uint64) reg

    val d0 : (arch, Id.t, uint64) reg
    val d1 : (arch, Id.t, uint64) reg
    val d2 : (arch, Id.t, uint64) reg
    val d3 : (arch, Id.t, uint64) reg
    val d4 : (arch, Id.t, uint64) reg
    val d5 : (arch, Id.t, uint64) reg
    val d6 : (arch, Id.t, uint64) reg
    val d7 : (arch, Id.t, uint64) reg
    val d8 : (arch, Id.t, uint64) reg
    val d9 : (arch, Id.t, uint64) reg
    val d10 : (arch, Id.t, uint64) reg
    val d11 : (arch, Id.t, uint64) reg
    val d12 : (arch, Id.t, uint64) reg
    val d13 : (arch, Id.t, uint64) reg
    val d14 : (arch, Id.t, uint64) reg
    val d15 : (arch, Id.t, uint64) reg
    val d16 : (arch, Id.t, uint64) reg
    val d17 : (arch, Id.t, uint64) reg
    val d18 : (arch, Id.t, uint64) reg
    val d19 : (arch, Id.t, uint64) reg
    val d20 : (arch, Id.t, uint64) reg
    val d21 : (arch, Id.t, uint64) reg
    val d22 : (arch, Id.t, uint64) reg
    val d23 : (arch, Id.t, uint64) reg
    val d24 : (arch, Id.t, uint64) reg
    val d25 : (arch, Id.t, uint64) reg
    val d26 : (arch, Id.t, uint64) reg
    val d27 : (arch, Id.t, uint64) reg
    val d28 : (arch, Id.t, uint64) reg
    val d29 : (arch, Id.t, uint64) reg
    val d30 : (arch, Id.t, uint64) reg
    val d31 : (arch, Id.t, uint64) reg

    val s0 : (arch, Id.t, uint32) reg
    val s1 : (arch, Id.t, uint32) reg
    val s2 : (arch, Id.t, uint32) reg
    val s3 : (arch, Id.t, uint32) reg
    val s4 : (arch, Id.t, uint32) reg
    val s5 : (arch, Id.t, uint32) reg
    val s6 : (arch, Id.t, uint32) reg
    val s7 : (arch, Id.t, uint32) reg
    val s8 : (arch, Id.t, uint32) reg
    val s9 : (arch, Id.t, uint32) reg
    val s10 : (arch, Id.t, uint32) reg
    val s11 : (arch, Id.t, uint32) reg
    val s12 : (arch, Id.t, uint32) reg
    val s13 : (arch, Id.t, uint32) reg
    val s14 : (arch, Id.t, uint32) reg
    val s15 : (arch, Id.t, uint32) reg
    val s16 : (arch, Id.t, uint32) reg
    val s17 : (arch, Id.t, uint32) reg
    val s18 : (arch, Id.t, uint32) reg
    val s19 : (arch, Id.t, uint32) reg
    val s20 : (arch, Id.t, uint32) reg
    val s21 : (arch, Id.t, uint32) reg
    val s22 : (arch, Id.t, uint32) reg
    val s23 : (arch, Id.t, uint32) reg
    val s24 : (arch, Id.t, uint32) reg
    val s25 : (arch, Id.t, uint32) reg
    val s26 : (arch, Id.t, uint32) reg
    val s27 : (arch, Id.t, uint32) reg
    val s28 : (arch, Id.t, uint32) reg
    val s29 : (arch, Id.t, uint32) reg
    val s30 : (arch, Id.t, uint32) reg
    val s31 : (arch, Id.t, uint32) reg

    val h0 : (arch, Id.t, uint16) reg
    val h1 : (arch, Id.t, uint16) reg
    val h2 : (arch, Id.t, uint16) reg
    val h3 : (arch, Id.t, uint16) reg
    val h4 : (arch, Id.t, uint16) reg
    val h5 : (arch, Id.t, uint16) reg
    val h6 : (arch, Id.t, uint16) reg
    val h7 : (arch, Id.t, uint16) reg
    val h8 : (arch, Id.t, uint16) reg
    val h9 : (arch, Id.t, uint16) reg
    val h10 : (arch, Id.t, uint16) reg
    val h11 : (arch, Id.t, uint16) reg
    val h12 : (arch, Id.t, uint16) reg
    val h13 : (arch, Id.t, uint16) reg
    val h14 : (arch, Id.t, uint16) reg
    val h15 : (arch, Id.t, uint16) reg
    val h16 : (arch, Id.t, uint16) reg
    val h17 : (arch, Id.t, uint16) reg
    val h18 : (arch, Id.t, uint16) reg
    val h19 : (arch, Id.t, uint16) reg
    val h20 : (arch, Id.t, uint16) reg
    val h21 : (arch, Id.t, uint16) reg
    val h22 : (arch, Id.t, uint16) reg
    val h23 : (arch, Id.t, uint16) reg
    val h24 : (arch, Id.t, uint16) reg
    val h25 : (arch, Id.t, uint16) reg
    val h26 : (arch, Id.t, uint16) reg
    val h27 : (arch, Id.t, uint16) reg
    val h28 : (arch, Id.t, uint16) reg
    val h29 : (arch, Id.t, uint16) reg
    val h30 : (arch, Id.t, uint16) reg
    val h31 : (arch, Id.t, uint16) reg

    val b0 : (arch, Id.t, uint8) reg
    val b1 : (arch, Id.t, uint8) reg
    val b2 : (arch, Id.t, uint8) reg
    val b3 : (arch, Id.t, uint8) reg
    val b4 : (arch, Id.t, uint8) reg
    val b5 : (arch, Id.t, uint8) reg
    val b6 : (arch, Id.t, uint8) reg
    val b7 : (arch, Id.t, uint8) reg
    val b8 : (arch, Id.t, uint8) reg
    val b9 : (arch, Id.t, uint8) reg
    val b10 : (arch, Id.t, uint8) reg
    val b11 : (arch, Id.t, uint8) reg
    val b12 : (arch, Id.t, uint8) reg
    val b13 : (arch, Id.t, uint8) reg
    val b14 : (arch, Id.t, uint8) reg
    val b15 : (arch, Id.t, uint8) reg
    val b16 : (arch, Id.t, uint8) reg
    val b17 : (arch, Id.t, uint8) reg
    val b18 : (arch, Id.t, uint8) reg
    val b19 : (arch, Id.t, uint8) reg
    val b20 : (arch, Id.t, uint8) reg
    val b21 : (arch, Id.t, uint8) reg
    val b22 : (arch, Id.t, uint8) reg
    val b23 : (arch, Id.t, uint8) reg
    val b24 : (arch, Id.t, uint8) reg
    val b25 : (arch, Id.t, uint8) reg
    val b26 : (arch, Id.t, uint8) reg
    val b27 : (arch, Id.t, uint8) reg
    val b28 : (arch, Id.t, uint8) reg
    val b29 : (arch, Id.t, uint8) reg
    val b30 : (arch, Id.t, uint8) reg
    val b31 : (arch, Id.t, uint8) reg

    val sp : (arch, Id.t, uint64) reg
    val lr : (arch, Id.t, uint64) reg
    val pc : (arch, Id.t, uint64) reg
    val nzcv : (arch, Id.t, uint32) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> ?endian : Endian.t -> unit -> (family, word) engine
end

module M68k : sig
  type arch   = Arch.m68k
  type family = Family.m68k
  type word   = uint32

  module Reg : sig
    module Id : module type of M68k.Reg.Id

    val a0 : (arch, Id.t, uint32) reg
    val a1 : (arch, Id.t, uint32) reg
    val a2 : (arch, Id.t, uint32) reg
    val a3 : (arch, Id.t, uint32) reg
    val a4 : (arch, Id.t, uint32) reg
    val a5 : (arch, Id.t, uint32) reg
    val a6 : (arch, Id.t, uint32) reg
    val a7 : (arch, Id.t, uint32) reg

    val d0 : (arch, Id.t, uint32) reg
    val d1 : (arch, Id.t, uint32) reg
    val d2 : (arch, Id.t, uint32) reg
    val d3 : (arch, Id.t, uint32) reg
    val d4 : (arch, Id.t, uint32) reg
    val d5 : (arch, Id.t, uint32) reg
    val d6 : (arch, Id.t, uint32) reg
    val d7 : (arch, Id.t, uint32) reg

    val pc : (arch, Id.t, uint32) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> unit -> (family, word) engine
end

module Mips64 : sig
  type arch = Arch.mips64
  type family = Family.mips
  type word = uint64

  module Reg : sig
    module Id : module type of Mips.M64.Reg.Id

    val r0 : (arch, Id.t, uint64) reg
    val r1 : (arch, Id.t, uint64) reg
    val r2 : (arch, Id.t, uint64) reg
    val r3 : (arch, Id.t, uint64) reg
    val r4 : (arch, Id.t, uint64) reg
    val r5 : (arch, Id.t, uint64) reg
    val r6 : (arch, Id.t, uint64) reg
    val r7 : (arch, Id.t, uint64) reg
    val r8 : (arch, Id.t, uint64) reg
    val r9 : (arch, Id.t, uint64) reg
    val r10 : (arch, Id.t, uint64) reg
    val r11 : (arch, Id.t, uint64) reg
    val r12 : (arch, Id.t, uint64) reg
    val r13 : (arch, Id.t, uint64) reg
    val r14 : (arch, Id.t, uint64) reg
    val r15 : (arch, Id.t, uint64) reg
    val r16 : (arch, Id.t, uint64) reg
    val r17 : (arch, Id.t, uint64) reg
    val r18 : (arch, Id.t, uint64) reg
    val r19 : (arch, Id.t, uint64) reg
    val r20 : (arch, Id.t, uint64) reg
    val r21 : (arch, Id.t, uint64) reg
    val r22 : (arch, Id.t, uint64) reg
    val r23 : (arch, Id.t, uint64) reg
    val r24 : (arch, Id.t, uint64) reg
    val r25 : (arch, Id.t, uint64) reg
    val r26 : (arch, Id.t, uint64) reg
    val r27 : (arch, Id.t, uint64) reg
    val r28 : (arch, Id.t, uint64) reg
    val r29 : (arch, Id.t, uint64) reg
    val r30 : (arch, Id.t, uint64) reg

	  val zero : (arch, Id.t, uint64) reg
	  val at : (arch, Id.t, uint64) reg
	  val v0 : (arch, Id.t, uint64) reg
	  val v1 : (arch, Id.t, uint64) reg
	  val a0 : (arch, Id.t, uint64) reg
	  val a1 : (arch, Id.t, uint64) reg
	  val a2 : (arch, Id.t, uint64) reg
	  val a3 : (arch, Id.t, uint64) reg
	  val t0 : (arch, Id.t, uint64) reg
	  val t1 : (arch, Id.t, uint64) reg
	  val t2 : (arch, Id.t, uint64) reg
	  val t3 : (arch, Id.t, uint64) reg
	  val t4 : (arch, Id.t, uint64) reg
	  val t5 : (arch, Id.t, uint64) reg
	  val t6 : (arch, Id.t, uint64) reg
	  val t7 : (arch, Id.t, uint64) reg
	  val s0 : (arch, Id.t, uint64) reg
	  val s1 : (arch, Id.t, uint64) reg
	  val s2 : (arch, Id.t, uint64) reg
	  val s3 : (arch, Id.t, uint64) reg
	  val s4 : (arch, Id.t, uint64) reg
	  val s5 : (arch, Id.t, uint64) reg
	  val s6 : (arch, Id.t, uint64) reg
	  val s7 : (arch, Id.t, uint64) reg
	  val t8 : (arch, Id.t, uint64) reg
	  val t9 : (arch, Id.t, uint64) reg
	  val k0 : (arch, Id.t, uint64) reg
	  val k1 : (arch, Id.t, uint64) reg
	  val gp : (arch, Id.t, uint64) reg
	  val sp : (arch, Id.t, uint64) reg
    val fp : (arch, Id.t, uint64) reg
    val s8 : (arch, Id.t, uint64) reg
	  val ra : (arch, Id.t, uint64) reg

    val pc : (arch, Id.t, uint64) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> ?endian : Endian.t -> unit -> (family, word) engine
end


module Mips : sig
  type arch = Arch.mips
  type family = Family.mips
  type word = uint32

  module Reg : sig
    module Id : module type of Mips.M32.Reg.Id

    val r0 : (arch, Id.t, uint32) reg
    val r1 : (arch, Id.t, uint32) reg
    val r2 : (arch, Id.t, uint32) reg
    val r3 : (arch, Id.t, uint32) reg
    val r4 : (arch, Id.t, uint32) reg
    val r5 : (arch, Id.t, uint32) reg
    val r6 : (arch, Id.t, uint32) reg
    val r7 : (arch, Id.t, uint32) reg
    val r8 : (arch, Id.t, uint32) reg
    val r9 : (arch, Id.t, uint32) reg
    val r10 : (arch, Id.t, uint32) reg
    val r11 : (arch, Id.t, uint32) reg
    val r12 : (arch, Id.t, uint32) reg
    val r13 : (arch, Id.t, uint32) reg
    val r14 : (arch, Id.t, uint32) reg
    val r15 : (arch, Id.t, uint32) reg
    val r16 : (arch, Id.t, uint32) reg
    val r17 : (arch, Id.t, uint32) reg
    val r18 : (arch, Id.t, uint32) reg
    val r19 : (arch, Id.t, uint32) reg
    val r20 : (arch, Id.t, uint32) reg
    val r21 : (arch, Id.t, uint32) reg
    val r22 : (arch, Id.t, uint32) reg
    val r23 : (arch, Id.t, uint32) reg
    val r24 : (arch, Id.t, uint32) reg
    val r25 : (arch, Id.t, uint32) reg
    val r26 : (arch, Id.t, uint32) reg
    val r27 : (arch, Id.t, uint32) reg
    val r28 : (arch, Id.t, uint32) reg
    val r29 : (arch, Id.t, uint32) reg
    val r30 : (arch, Id.t, uint32) reg

	  val zero : (arch, Id.t, uint32) reg
	  val at : (arch, Id.t, uint32) reg
	  val v0 : (arch, Id.t, uint32) reg
	  val v1 : (arch, Id.t, uint32) reg
	  val a0 : (arch, Id.t, uint32) reg
	  val a1 : (arch, Id.t, uint32) reg
	  val a2 : (arch, Id.t, uint32) reg
	  val a3 : (arch, Id.t, uint32) reg
	  val t0 : (arch, Id.t, uint32) reg
	  val t1 : (arch, Id.t, uint32) reg
	  val t2 : (arch, Id.t, uint32) reg
	  val t3 : (arch, Id.t, uint32) reg
	  val t4 : (arch, Id.t, uint32) reg
	  val t5 : (arch, Id.t, uint32) reg
	  val t6 : (arch, Id.t, uint32) reg
	  val t7 : (arch, Id.t, uint32) reg
	  val s0 : (arch, Id.t, uint32) reg
	  val s1 : (arch, Id.t, uint32) reg
	  val s2 : (arch, Id.t, uint32) reg
	  val s3 : (arch, Id.t, uint32) reg
	  val s4 : (arch, Id.t, uint32) reg
	  val s5 : (arch, Id.t, uint32) reg
	  val s6 : (arch, Id.t, uint32) reg
	  val s7 : (arch, Id.t, uint32) reg
	  val t8 : (arch, Id.t, uint32) reg
	  val t9 : (arch, Id.t, uint32) reg
	  val k0 : (arch, Id.t, uint32) reg
	  val k1 : (arch, Id.t, uint32) reg
	  val gp : (arch, Id.t, uint32) reg
	  val sp : (arch, Id.t, uint32) reg
    val fp : (arch, Id.t, uint32) reg
    val s8 : (arch, Id.t, uint32) reg
	  val ra : (arch, Id.t, uint32) reg

    val pc : (arch, Id.t, uint32) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> ?endian : Endian.t -> unit -> (family, word) engine
end

module Sparc64 : sig
  type arch = Arch.sparc64
  type family = Family.sparc
  type word = uint64

  module Reg : sig
    module Id : module type of Sparc.M64.Reg.Id

    val g0 : (arch, Id.t, uint64) reg
    val g1 : (arch, Id.t, uint64) reg
    val g2 : (arch, Id.t, uint64) reg
    val g3 : (arch, Id.t, uint64) reg
    val g4 : (arch, Id.t, uint64) reg
    val g5 : (arch, Id.t, uint64) reg
    val g6 : (arch, Id.t, uint64) reg
    val g7 : (arch, Id.t, uint64) reg

    val o0 : (arch, Id.t, uint64) reg
    val o1 : (arch, Id.t, uint64) reg
    val o2 : (arch, Id.t, uint64) reg
    val o3 : (arch, Id.t, uint64) reg
    val o4 : (arch, Id.t, uint64) reg
    val o5 : (arch, Id.t, uint64) reg
    val o6 : (arch, Id.t, uint64) reg
    val o7 : (arch, Id.t, uint64) reg

    val l0 : (arch, Id.t, uint64) reg
    val l1 : (arch, Id.t, uint64) reg
    val l2 : (arch, Id.t, uint64) reg
    val l3 : (arch, Id.t, uint64) reg
    val l4 : (arch, Id.t, uint64) reg
    val l5 : (arch, Id.t, uint64) reg
    val l6 : (arch, Id.t, uint64) reg
    val l7 : (arch, Id.t, uint64) reg

    val i0 : (arch, Id.t, uint64) reg
    val i1 : (arch, Id.t, uint64) reg
    val i2 : (arch, Id.t, uint64) reg
    val i3 : (arch, Id.t, uint64) reg
    val i4 : (arch, Id.t, uint64) reg
    val i5 : (arch, Id.t, uint64) reg
    val i6 : (arch, Id.t, uint64) reg
    val i7 : (arch, Id.t, uint64) reg

    val pc : (arch, Id.t, uint64) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> unit -> (family, word) engine
end

module Sparc : sig
  type arch = Arch.sparc
  type family = Family.sparc
  type word = uint32

  module Reg : sig
    module Id : module type of Sparc.M32.Reg.Id

    val g0 : (arch, Id.t, uint32) reg
    val g1 : (arch, Id.t, uint32) reg
    val g2 : (arch, Id.t, uint32) reg
    val g3 : (arch, Id.t, uint32) reg
    val g4 : (arch, Id.t, uint32) reg
    val g5 : (arch, Id.t, uint32) reg
    val g6 : (arch, Id.t, uint32) reg
    val g7 : (arch, Id.t, uint32) reg

    val o0 : (arch, Id.t, uint32) reg
    val o1 : (arch, Id.t, uint32) reg
    val o2 : (arch, Id.t, uint32) reg
    val o3 : (arch, Id.t, uint32) reg
    val o4 : (arch, Id.t, uint32) reg
    val o5 : (arch, Id.t, uint32) reg
    val o6 : (arch, Id.t, uint32) reg
    val o7 : (arch, Id.t, uint32) reg

    val l0 : (arch, Id.t, uint32) reg
    val l1 : (arch, Id.t, uint32) reg
    val l2 : (arch, Id.t, uint32) reg
    val l3 : (arch, Id.t, uint32) reg
    val l4 : (arch, Id.t, uint32) reg
    val l5 : (arch, Id.t, uint32) reg
    val l6 : (arch, Id.t, uint32) reg
    val l7 : (arch, Id.t, uint32) reg

    val i0 : (arch, Id.t, uint32) reg
    val i1 : (arch, Id.t, uint32) reg
    val i2 : (arch, Id.t, uint32) reg
    val i3 : (arch, Id.t, uint32) reg
    val i4 : (arch, Id.t, uint32) reg
    val i5 : (arch, Id.t, uint32) reg
    val i6 : (arch, Id.t, uint32) reg
    val i7 : (arch, Id.t, uint32) reg

    val pc : (arch, Id.t, uint32) reg

    val read   : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w
    val write  : (family, word) engine -> (arch, Id.t, 'w) reg -> 'w -> unit
  end

  val create : ?mode : arch Mode.t -> unit -> (family, word) engine
end

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

  val block  : (('f, 'w) engine -> int64 -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
  val code   : (('f, 'w) engine -> int64 -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
  val intr   : (('f, 'w) engine -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
  val mem    : (('f, 'w) engine -> Memory.Access.Valid.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> Memory.Access.Valid.t -> ('f, 'w, 'v) callback
  val mem_ev : (('f, 'w) engine -> Memory.Access.Invalid.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> Memory.Access.Invalid.t -> ('f, 'w, 'v) callback

  val add    : ?saddr : 'w -> ?eaddr : 'w -> ('f, 'w) engine -> ('f, 'w, 'v) callback -> 'v -> ('f, 'w) handle
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
