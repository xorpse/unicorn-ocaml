open Stdint
open Types

type arch   = Arch.aarch64
type family = Family.arm
type word   = uint64

(* Aarch32 exists; is it possible in Unicorn to switch to
   this mode via MODE_32 option in the setup?

   The following issue seems to imply it's not possible:
   https://github.com/unicorn-engine/unicorn/issues/1070

   On the master branch (https://github.com/unicorn-engine/unicorn/blob/8621bca53758532ad6a4ec5d17684fcdb9923cc6/uc.c#L208)
   it seems to be prohibited when creating a handle.
*)

module Reg = struct
  module Id = Arm64_const.Reg

  let x0 = `AARCH64, Id.x0, Size.W64
  let x1 = `AARCH64, Id.x1, Size.W64
  let x2 = `AARCH64, Id.x2, Size.W64
  let x3 = `AARCH64, Id.x3, Size.W64
  let x4 = `AARCH64, Id.x4, Size.W64
  let x5 = `AARCH64, Id.x5, Size.W64
  let x6 = `AARCH64, Id.x6, Size.W64
  let x7 = `AARCH64, Id.x7, Size.W64
  let x8 = `AARCH64, Id.x8, Size.W64
  let x9 = `AARCH64, Id.x9, Size.W64
  let x10 = `AARCH64, Id.x10, Size.W64
  let x11 = `AARCH64, Id.x11, Size.W64
  let x12 = `AARCH64, Id.x12, Size.W64
  let x13 = `AARCH64, Id.x13, Size.W64
  let x14 = `AARCH64, Id.x14, Size.W64
  let x15 = `AARCH64, Id.x15, Size.W64
  let x16 = `AARCH64, Id.x16, Size.W64
  let x17 = `AARCH64, Id.x17, Size.W64
  let x18 = `AARCH64, Id.x18, Size.W64
  let x19 = `AARCH64, Id.x19, Size.W64
  let x20 = `AARCH64, Id.x20, Size.W64
  let x21 = `AARCH64, Id.x21, Size.W64
  let x22 = `AARCH64, Id.x22, Size.W64
  let x23 = `AARCH64, Id.x23, Size.W64
  let x24 = `AARCH64, Id.x24, Size.W64
  let x25 = `AARCH64, Id.x25, Size.W64
  let x26 = `AARCH64, Id.x26, Size.W64
  let x27 = `AARCH64, Id.x27, Size.W64
  let x28 = `AARCH64, Id.x28, Size.W64
  let x29 = `AARCH64, Id.x29, Size.W64
  let x30 = `AARCH64, Id.x30, Size.W64

  let d0 = `AARCH64, Id.d0, Size.W64
  let d1 = `AARCH64, Id.d1, Size.W64
  let d2 = `AARCH64, Id.d2, Size.W64
  let d3 = `AARCH64, Id.d3, Size.W64
  let d4 = `AARCH64, Id.d4, Size.W64
  let d5 = `AARCH64, Id.d5, Size.W64
  let d6 = `AARCH64, Id.d6, Size.W64
  let d7 = `AARCH64, Id.d7, Size.W64
  let d8 = `AARCH64, Id.d8, Size.W64
  let d9 = `AARCH64, Id.d9, Size.W64
  let d10 = `AARCH64, Id.d10, Size.W64
  let d11 = `AARCH64, Id.d11, Size.W64
  let d12 = `AARCH64, Id.d12, Size.W64
  let d13 = `AARCH64, Id.d13, Size.W64
  let d14 = `AARCH64, Id.d14, Size.W64
  let d15 = `AARCH64, Id.d15, Size.W64
  let d16 = `AARCH64, Id.d16, Size.W64
  let d17 = `AARCH64, Id.d17, Size.W64
  let d18 = `AARCH64, Id.d18, Size.W64
  let d19 = `AARCH64, Id.d19, Size.W64
  let d20 = `AARCH64, Id.d20, Size.W64
  let d21 = `AARCH64, Id.d21, Size.W64
  let d22 = `AARCH64, Id.d22, Size.W64
  let d23 = `AARCH64, Id.d23, Size.W64
  let d24 = `AARCH64, Id.d24, Size.W64
  let d25 = `AARCH64, Id.d25, Size.W64
  let d26 = `AARCH64, Id.d26, Size.W64
  let d27 = `AARCH64, Id.d27, Size.W64
  let d28 = `AARCH64, Id.d28, Size.W64
  let d29 = `AARCH64, Id.d29, Size.W64
  let d30 = `AARCH64, Id.d30, Size.W64
  let d31 = `AARCH64, Id.d31, Size.W64

  let s0 = `AARCH64, Id.s0, Size.W32
  let s1 = `AARCH64, Id.s1, Size.W32
  let s2 = `AARCH64, Id.s2, Size.W32
  let s3 = `AARCH64, Id.s3, Size.W32
  let s4 = `AARCH64, Id.s4, Size.W32
  let s5 = `AARCH64, Id.s5, Size.W32
  let s6 = `AARCH64, Id.s6, Size.W32
  let s7 = `AARCH64, Id.s7, Size.W32
  let s8 = `AARCH64, Id.s8, Size.W32
  let s9 = `AARCH64, Id.s9, Size.W32
  let s10 = `AARCH64, Id.s10, Size.W32
  let s11 = `AARCH64, Id.s11, Size.W32
  let s12 = `AARCH64, Id.s12, Size.W32
  let s13 = `AARCH64, Id.s13, Size.W32
  let s14 = `AARCH64, Id.s14, Size.W32
  let s15 = `AARCH64, Id.s15, Size.W32
  let s16 = `AARCH64, Id.s16, Size.W32
  let s17 = `AARCH64, Id.s17, Size.W32
  let s18 = `AARCH64, Id.s18, Size.W32
  let s19 = `AARCH64, Id.s19, Size.W32
  let s20 = `AARCH64, Id.s20, Size.W32
  let s21 = `AARCH64, Id.s21, Size.W32
  let s22 = `AARCH64, Id.s22, Size.W32
  let s23 = `AARCH64, Id.s23, Size.W32
  let s24 = `AARCH64, Id.s24, Size.W32
  let s25 = `AARCH64, Id.s25, Size.W32
  let s26 = `AARCH64, Id.s26, Size.W32
  let s27 = `AARCH64, Id.s27, Size.W32
  let s28 = `AARCH64, Id.s28, Size.W32
  let s29 = `AARCH64, Id.s29, Size.W32
  let s30 = `AARCH64, Id.s30, Size.W32
  let s31 = `AARCH64, Id.s31, Size.W32

  let h0 = `AARCH64, Id.h0, Size.W16
  let h1 = `AARCH64, Id.h1, Size.W16
  let h2 = `AARCH64, Id.h2, Size.W16
  let h3 = `AARCH64, Id.h3, Size.W16
  let h4 = `AARCH64, Id.h4, Size.W16
  let h5 = `AARCH64, Id.h5, Size.W16
  let h6 = `AARCH64, Id.h6, Size.W16
  let h7 = `AARCH64, Id.h7, Size.W16
  let h8 = `AARCH64, Id.h8, Size.W16
  let h9 = `AARCH64, Id.h9, Size.W16
  let h10 = `AARCH64, Id.h10, Size.W16
  let h11 = `AARCH64, Id.h11, Size.W16
  let h12 = `AARCH64, Id.h12, Size.W16
  let h13 = `AARCH64, Id.h13, Size.W16
  let h14 = `AARCH64, Id.h14, Size.W16
  let h15 = `AARCH64, Id.h15, Size.W16
  let h16 = `AARCH64, Id.h16, Size.W16
  let h17 = `AARCH64, Id.h17, Size.W16
  let h18 = `AARCH64, Id.h18, Size.W16
  let h19 = `AARCH64, Id.h19, Size.W16
  let h20 = `AARCH64, Id.h20, Size.W16
  let h21 = `AARCH64, Id.h21, Size.W16
  let h22 = `AARCH64, Id.h22, Size.W16
  let h23 = `AARCH64, Id.h23, Size.W16
  let h24 = `AARCH64, Id.h24, Size.W16
  let h25 = `AARCH64, Id.h25, Size.W16
  let h26 = `AARCH64, Id.h26, Size.W16
  let h27 = `AARCH64, Id.h27, Size.W16
  let h28 = `AARCH64, Id.h28, Size.W16
  let h29 = `AARCH64, Id.h29, Size.W16
  let h30 = `AARCH64, Id.h30, Size.W16
  let h31 = `AARCH64, Id.h31, Size.W16

  let b0 = `AARCH64, Id.b0, Size.W8
  let b1 = `AARCH64, Id.b1, Size.W8
  let b2 = `AARCH64, Id.b2, Size.W8
  let b3 = `AARCH64, Id.b3, Size.W8
  let b4 = `AARCH64, Id.b4, Size.W8
  let b5 = `AARCH64, Id.b5, Size.W8
  let b6 = `AARCH64, Id.b6, Size.W8
  let b7 = `AARCH64, Id.b7, Size.W8
  let b8 = `AARCH64, Id.b8, Size.W8
  let b9 = `AARCH64, Id.b9, Size.W8
  let b10 = `AARCH64, Id.b10, Size.W8
  let b11 = `AARCH64, Id.b11, Size.W8
  let b12 = `AARCH64, Id.b12, Size.W8
  let b13 = `AARCH64, Id.b13, Size.W8
  let b14 = `AARCH64, Id.b14, Size.W8
  let b15 = `AARCH64, Id.b15, Size.W8
  let b16 = `AARCH64, Id.b16, Size.W8
  let b17 = `AARCH64, Id.b17, Size.W8
  let b18 = `AARCH64, Id.b18, Size.W8
  let b19 = `AARCH64, Id.b19, Size.W8
  let b20 = `AARCH64, Id.b20, Size.W8
  let b21 = `AARCH64, Id.b21, Size.W8
  let b22 = `AARCH64, Id.b22, Size.W8
  let b23 = `AARCH64, Id.b23, Size.W8
  let b24 = `AARCH64, Id.b24, Size.W8
  let b25 = `AARCH64, Id.b25, Size.W8
  let b26 = `AARCH64, Id.b26, Size.W8
  let b27 = `AARCH64, Id.b27, Size.W8
  let b28 = `AARCH64, Id.b28, Size.W8
  let b29 = `AARCH64, Id.b29, Size.W8
  let b30 = `AARCH64, Id.b30, Size.W8
  let b31 = `AARCH64, Id.b31, Size.W8

  let sp = `AARCH64, Id.sp, Size.W64
  let lr = `AARCH64, Id.lr, Size.W64
  let pc = `AARCH64, Id.pc, Size.W64
  let nzcv = `AARCH64, Id.nzcv, Size.W32

  (* TODO: vX/qX registers *)

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
  let h = create_ffi ~arch:Uc_const.Arch.arm64 ~mode in
  engine ~family:`ARM ~endian:endian ~word_size:Size.W64 h
