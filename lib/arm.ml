open Stdint
open Types

type arch   = Arch.arm
type family = Family.arm
type word   = uint32

module Reg = struct
  module Id = Arm_const.Reg

  let r0 = `ARM, Id.r0, Size.W32
  let r1 = `ARM, Id.r1, Size.W32
  let r2 = `ARM, Id.r2, Size.W32
  let r3 = `ARM, Id.r3, Size.W32
  let r4 = `ARM, Id.r4, Size.W32
  let r5 = `ARM, Id.r5, Size.W32
  let r6 = `ARM, Id.r6, Size.W32
  let r7 = `ARM, Id.r7, Size.W32
  let r8 = `ARM, Id.r8, Size.W32
  let r9 = `ARM, Id.r9, Size.W32
  let sb = `ARM, Id.sb, Size.W32
  let r10 = `ARM, Id.r10, Size.W32
  let sl = `ARM, Id.sl, Size.W32
  let r11 = `ARM, Id.r11, Size.W32
  let fp = `ARM, Id.fp, Size.W32
  let r12 = `ARM, Id.r12, Size.W32
  let ip = `ARM, Id.ip, Size.W32
  let sp = `ARM, Id.sp, Size.W32
  let lr = `ARM, Id.lr, Size.W32
  let pc = `ARM, Id.pc, Size.W32
  let c1_c0_2 = `ARM, Id.c1_c0_2, Size.W32
  let c13_c0_3 = `ARM, Id.c13_c0_3, Size.W32
  let apsr = `ARM, Id.apsr, Size.W32
  let cpsr = `ARM, Id.cpsr, Size.W32
  let spsr = `ARM, Id.spsr, Size.W32
  let fpsid = `ARM, Id.fpsid, Size.W32
  let fpscr = `ARM, Id.fpscr, Size.W32
  let fpexc = `ARM, Id.fpexc, Size.W32
  let mvfr0 = `ARM, Id.mvfr0, Size.W32
  let mvfr1 = `ARM, Id.mvfr1, Size.W32
  let s0 = `ARM, Id.s0, Size.W32
  let d0 = `ARM, Id.d0, Size.W64
  let s1 = `ARM, Id.s1, Size.W32
  let s2 = `ARM, Id.s2, Size.W32
  let d1 = `ARM, Id.d1, Size.W64
  let s3 = `ARM, Id.s3, Size.W32
  let s4 = `ARM, Id.s4, Size.W32
  let d2 = `ARM, Id.d2, Size.W64
  let s5 = `ARM, Id.s5, Size.W32
  let s6 = `ARM, Id.s6, Size.W32
  let d3 = `ARM, Id.d3, Size.W64
  let s7 = `ARM, Id.s7, Size.W32
  let s8 = `ARM, Id.s8, Size.W32
  let d4 = `ARM, Id.d4, Size.W64
  let s9 = `ARM, Id.s9, Size.W32
  let s10 = `ARM, Id.s10, Size.W32
  let d5 = `ARM, Id.d5, Size.W64
  let s11 = `ARM, Id.s11, Size.W32
  let s12 = `ARM, Id.s12, Size.W32
  let d6 = `ARM, Id.d6, Size.W64
  let s13 = `ARM, Id.s13, Size.W32
  let s14 = `ARM, Id.s14, Size.W32
  let d7 = `ARM, Id.d7, Size.W64
  let s15 = `ARM, Id.s15, Size.W32
  let s16 = `ARM, Id.s16, Size.W32
  let d8 = `ARM, Id.d8, Size.W64
  let s17 = `ARM, Id.s17, Size.W32
  let s18 = `ARM, Id.s18, Size.W32
  let d9 = `ARM, Id.d9, Size.W64
  let s19 = `ARM, Id.s19, Size.W32
  let s20 = `ARM, Id.s20, Size.W32
  let d10 = `ARM, Id.d10, Size.W64
  let s21 = `ARM, Id.s21, Size.W32
  let s22 = `ARM, Id.s22, Size.W32
  let d11 = `ARM, Id.d11, Size.W64
  let s23 = `ARM, Id.s23, Size.W32
  let s24 = `ARM, Id.s24, Size.W32
  let d12 = `ARM, Id.d12, Size.W64
  let s25 = `ARM, Id.s25, Size.W32
  let s26 = `ARM, Id.s26, Size.W32
  let d13 = `ARM, Id.d13, Size.W64
  let s27 = `ARM, Id.s27, Size.W32
  let s28 = `ARM, Id.s28, Size.W32
  let d14 = `ARM, Id.d14, Size.W64
  let s29 = `ARM, Id.s29, Size.W32
  let s30 = `ARM, Id.s30, Size.W32
  let d15 = `ARM, Id.d15, Size.W64
  let s31 = `ARM, Id.s31, Size.W32
  let d16 = `ARM, Id.d16, Size.W64
  let d17 = `ARM, Id.d17, Size.W64
  let d18 = `ARM, Id.d18, Size.W64
  let d19 = `ARM, Id.d19, Size.W64
  let d20 = `ARM, Id.d20, Size.W64
  let d21 = `ARM, Id.d21, Size.W64
  let d22 = `ARM, Id.d22, Size.W64
  let d23 = `ARM, Id.d23, Size.W64
  let d24 = `ARM, Id.d24, Size.W64
  let d25 = `ARM, Id.d25, Size.W64
  let d26 = `ARM, Id.d26, Size.W64
  let d27 = `ARM, Id.d27, Size.W64
  let d28 = `ARM, Id.d28, Size.W64
  let d29 = `ARM, Id.d29, Size.W64
  let d30 = `ARM, Id.d30, Size.W64
  let d31 = `ARM, Id.d31, Size.W64

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

let create ?mode ?endian () =
  let endian = match endian with Some e -> e | None -> Endian.Little in
  let default_mode = match endian with
    | Endian.Big -> Mode.big_endian
    | Endian.Little -> Mode.little_endian
  in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.arm ~mode in
  engine ~family:`ARM ~endian:endian ~word_size:Size.W32 h
