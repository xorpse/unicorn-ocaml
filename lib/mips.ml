open Stdint
open Types

module Make (A : sig type arch val arch : arch val mode : arch Mode.t end) (W : Size.S) = struct
  type arch = A.arch
  type family = Family.mips
  type word = W.word

  module Reg = struct
    module Id = Mips_const.Reg

    let r0 = A.arch, Id.reg_0, W.word
    let r1 = A.arch, Id.reg_1, W.word
    let r2 = A.arch, Id.reg_2, W.word
    let r3 = A.arch, Id.reg_3, W.word
    let r4 = A.arch, Id.reg_4, W.word
    let r5 = A.arch, Id.reg_5, W.word
    let r6 = A.arch, Id.reg_6, W.word
    let r7 = A.arch, Id.reg_7, W.word
    let r8 = A.arch, Id.reg_8, W.word
    let r9 = A.arch, Id.reg_9, W.word
    let r10 = A.arch, Id.reg_10, W.word
    let r11 = A.arch, Id.reg_11, W.word
    let r12 = A.arch, Id.reg_12, W.word
    let r13 = A.arch, Id.reg_13, W.word
    let r14 = A.arch, Id.reg_14, W.word
    let r15 = A.arch, Id.reg_15, W.word
    let r16 = A.arch, Id.reg_16, W.word
    let r17 = A.arch, Id.reg_17, W.word
    let r18 = A.arch, Id.reg_18, W.word
    let r19 = A.arch, Id.reg_19, W.word
    let r20 = A.arch, Id.reg_20, W.word
    let r21 = A.arch, Id.reg_21, W.word
    let r22 = A.arch, Id.reg_22, W.word
    let r23 = A.arch, Id.reg_23, W.word
    let r24 = A.arch, Id.reg_24, W.word
    let r25 = A.arch, Id.reg_25, W.word
    let r26 = A.arch, Id.reg_26, W.word
    let r27 = A.arch, Id.reg_27, W.word
    let r28 = A.arch, Id.reg_28, W.word
    let r29 = A.arch, Id.reg_29, W.word
    let r30 = A.arch, Id.reg_30, W.word

	  let zero = A.arch, Id.zero, W.word
	  let at = A.arch, Id.at, W.word
	  let v0 = A.arch, Id.v0, W.word
	  let v1 = A.arch, Id.v1, W.word
	  let a0 = A.arch, Id.a0, W.word
	  let a1 = A.arch, Id.a1, W.word
	  let a2 = A.arch, Id.a2, W.word
	  let a3 = A.arch, Id.a3, W.word
	  let t0 = A.arch, Id.t0, W.word
	  let t1 = A.arch, Id.t1, W.word
	  let t2 = A.arch, Id.t2, W.word
	  let t3 = A.arch, Id.t3, W.word
	  let t4 = A.arch, Id.t4, W.word
	  let t5 = A.arch, Id.t5, W.word
	  let t6 = A.arch, Id.t6, W.word
	  let t7 = A.arch, Id.t7, W.word
	  let s0 = A.arch, Id.s0, W.word
	  let s1 = A.arch, Id.s1, W.word
	  let s2 = A.arch, Id.s2, W.word
	  let s3 = A.arch, Id.s3, W.word
	  let s4 = A.arch, Id.s4, W.word
	  let s5 = A.arch, Id.s5, W.word
	  let s6 = A.arch, Id.s6, W.word
	  let s7 = A.arch, Id.s7, W.word
	  let t8 = A.arch, Id.t8, W.word
	  let t9 = A.arch, Id.t9, W.word
	  let k0 = A.arch, Id.k0, W.word
	  let k1 = A.arch, Id.k1, W.word
	  let gp = A.arch, Id.gp, W.word
	  let sp = A.arch, Id.sp, W.word
    let fp = A.arch, Id.fp, W.word
    let s8 = A.arch, Id.s8, W.word
	  let ra = A.arch, Id.ra, W.word

    let pc = A.arch, Id.pc, W.word

    let read (type rs) e (r : (arch, Id.t, rs) reg) : rs =
      let h = Types.handle e in
      let (_, r, s) = r in
      match s with
      | Size.W8 -> Types.reg_read_uint8_ffi h (r :> int)
      | Size.W16 -> Types.reg_read_uint16_ffi h (r :> int)
      | Size.W32 -> Types.reg_read_uint32_ffi h (r :> int)
      | Size.W64 -> Types.reg_read_uint64_ffi h (r :> int)

    let write (type rs) e (r : (arch, Id.t, rs) reg) (v : rs) =
      let h = Types.handle e in
      let (_, r, s) = r in
      match s with
      | Size.W8 -> Types.reg_write_uint8_ffi h (r :> int) v
      | Size.W16 -> Types.reg_write_uint16_ffi h (r :> int) v
      | Size.W32 -> Types.reg_write_uint32_ffi h (r :> int) v
      | Size.W64 -> Types.reg_write_uint64_ffi h (r :> int) v
  end

  let create ?(mode : arch Mode.t option) ?endian () =
    let endian = match endian with Some e -> e | None -> Endian.Little in
    let default_mode = Mode.(A.mode & match endian with
      | Endian.Big -> big_endian
      | Endian.Little -> little_endian)
    in
    let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
    let h = create_ffi ~arch:Uc_const.Arch.mips ~mode in
    engine ~family:`MIPS ~endian:endian ~word_size:W.word h
end

module M32 = struct
  include Make
      (struct type arch = Arch.mips let arch = `MIPS let mode = Mode.mode_32 end)
      (struct type word = uint32 let word = Size.W32 end)
end

module M64 = struct
  include Make
      (struct type arch = Arch.mips64 let arch = `MIPS64 let mode = Mode.mode_64 end)
      (struct type word = uint64 let word = Size.W64 end)
end
