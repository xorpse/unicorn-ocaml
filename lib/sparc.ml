open Stdint
open Types

module Make (A : sig type arch val arch : arch val mode : arch Mode.t end) (W : Size.S) = struct
  type arch = A.arch
  type family = Family.sparc
  type word = W.word

  module Reg = struct
    module Id = Sparc_const.Reg

    let g0 = A.arch, Id.g0, W.word
    let g1 = A.arch, Id.g1, W.word
    let g2 = A.arch, Id.g2, W.word
    let g3 = A.arch, Id.g3, W.word
    let g4 = A.arch, Id.g4, W.word
    let g5 = A.arch, Id.g5, W.word
    let g6 = A.arch, Id.g6, W.word
    let g7 = A.arch, Id.g7, W.word

    let o0 = A.arch, Id.o0, W.word
    let o1 = A.arch, Id.o1, W.word
    let o2 = A.arch, Id.o2, W.word
    let o3 = A.arch, Id.o3, W.word
    let o4 = A.arch, Id.o4, W.word
    let o5 = A.arch, Id.o5, W.word
    let o6 = A.arch, Id.o6, W.word
    let o7 = A.arch, Id.o7, W.word

    let l0 = A.arch, Id.l0, W.word
    let l1 = A.arch, Id.l1, W.word
    let l2 = A.arch, Id.l2, W.word
    let l3 = A.arch, Id.l3, W.word
    let l4 = A.arch, Id.l4, W.word
    let l5 = A.arch, Id.l5, W.word
    let l6 = A.arch, Id.l6, W.word
    let l7 = A.arch, Id.l7, W.word

    let i0 = A.arch, Id.i0, W.word
    let i1 = A.arch, Id.i1, W.word
    let i2 = A.arch, Id.i2, W.word
    let i3 = A.arch, Id.i3, W.word
    let i4 = A.arch, Id.i4, W.word
    let i5 = A.arch, Id.i5, W.word
    let i6 = A.arch, Id.i6, W.word
    let i7 = A.arch, Id.i7, W.word

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

  let create ?(mode : arch Mode.t option) () =
    let default_mode = Mode.(big_endian & A.mode) in
    let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
    let h = create_ffi ~arch:Uc_const.Arch.sparc ~mode in
    engine ~family:`SPARC ~endian:Endian.Big.endian ~word_size:W.word h
end

module M32 = struct
  include Make
      (struct type arch = Arch.sparc let arch = `SPARC let mode = Mode.mode_32 end)
      (struct type word = uint32 let word = Size.W32 end)
end

module M64 = struct
  include Make
      (struct type arch = Arch.sparc64 let arch = `SPARC64 let mode = Mode.mode_64 end)
      (struct type word = uint64 let word = Size.W64 end)
end
