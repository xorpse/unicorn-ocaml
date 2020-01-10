open Stdint
open Types

type arch   = Arch.m68k
type family = Family.m68k
type word   = uint32

module Reg = struct
  module Id = M68k_const.Reg

  let a0 = `M68K, Id.a0, Size.W32
  let a1 = `M68K, Id.a1, Size.W32
  let a2 = `M68K, Id.a2, Size.W32
  let a3 = `M68K, Id.a3, Size.W32
  let a4 = `M68K, Id.a4, Size.W32
  let a5 = `M68K, Id.a5, Size.W32
  let a6 = `M68K, Id.a6, Size.W32
  let a7 = `M68K, Id.a7, Size.W32

  let d0 = `M68K, Id.d0, Size.W32
  let d1 = `M68K, Id.d1, Size.W32
  let d2 = `M68K, Id.d2, Size.W32
  let d3 = `M68K, Id.d3, Size.W32
  let d4 = `M68K, Id.d4, Size.W32
  let d5 = `M68K, Id.d5, Size.W32
  let d6 = `M68K, Id.d6, Size.W32
  let d7 = `M68K, Id.d7, Size.W32

  let pc = `M68K, Id.pc, Size.W32

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

let create ?mode () =
  let default_mode = Mode.little_endian in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.m68k ~mode in
  engine ~family:`M68K ~endian:Endian.Little.endian ~word_size:Size.W32 h
