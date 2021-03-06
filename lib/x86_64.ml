open Stdint
open Types

type arch   = Arch.x86_64
type family = Family.x86
type word   = uint64

module Insn = X86_const.Insn
module Reg = struct
  module Id = X86_const.Reg

  let al     = `X86_64, Id.al, Size.W8
  let ax     = `X86_64, Id.ax, Size.W16
  let eax    = `X86_64, Id.eax, Size.W32
  let rax    = `X86_64, Id.rax, Size.W64
  let ah     = `X86_64, Id.ah, Size.W8
  let cl     = `X86_64, Id.cl, Size.W8
  let cx     = `X86_64, Id.cx, Size.W16
  let ecx    = `X86_64, Id.ecx, Size.W32
  let rcx    = `X86_64, Id.rcx, Size.W64
  let ch     = `X86_64, Id.ch, Size.W8
  let dl     = `X86_64, Id.dl, Size.W8
  let dx     = `X86_64, Id.dx, Size.W16
  let edx    = `X86_64, Id.edx, Size.W32
  let rdx    = `X86_64, Id.rdx, Size.W64
  let dh     = `X86_64, Id.dh, Size.W8
  let bl     = `X86_64, Id.bl, Size.W8
  let bx     = `X86_64, Id.bx, Size.W16
  let ebx    = `X86_64, Id.ebx, Size.W32
  let rbx    = `X86_64, Id.rbx, Size.W64
  let bh     = `X86_64, Id.bh, Size.W8
  let spl    = `X86_64, Id.spl, Size.W8
  let sp     = `X86_64, Id.sp, Size.W16
  let esp    = `X86_64, Id.esp, Size.W32
  let rsp    = `X86_64, Id.rsp, Size.W64
  let bpl    = `X86_64, Id.bpl, Size.W8
  let bp     = `X86_64, Id.bp, Size.W16
  let ebp    = `X86_64, Id.ebp, Size.W32
  let rbp    = `X86_64, Id.rbp, Size.W64
  let sil    = `X86_64, Id.sil, Size.W8
  let si     = `X86_64, Id.si, Size.W16
  let esi    = `X86_64, Id.esi, Size.W32
  let rsi    = `X86_64, Id.rsi, Size.W64
  let dil    = `X86_64, Id.dil, Size.W8
  let di     = `X86_64, Id.di, Size.W16
  let edi    = `X86_64, Id.edi, Size.W32
  let rdi    = `X86_64, Id.rdi, Size.W64
  let r8b    = `X86_64, Id.r8b, Size.W8
  let r8w    = `X86_64, Id.r8w, Size.W16
  let r8d    = `X86_64, Id.r8d, Size.W32
  let r8     = `X86_64, Id.r8, Size.W64
  let r9b    = `X86_64, Id.r9b, Size.W8
  let r9w    = `X86_64, Id.r9w, Size.W16
  let r9d    = `X86_64, Id.r9d, Size.W32
  let r9     = `X86_64, Id.r9, Size.W64
  let r10b   = `X86_64, Id.r10b, Size.W8
  let r10w   = `X86_64, Id.r10w, Size.W16
  let r10d   = `X86_64, Id.r10d, Size.W32
  let r10    = `X86_64, Id.r10, Size.W64
  let r11b   = `X86_64, Id.r11b, Size.W8
  let r11w   = `X86_64, Id.r11w, Size.W16
  let r11d   = `X86_64, Id.r11d, Size.W32
  let r11    = `X86_64, Id.r11, Size.W64
  let r12b   = `X86_64, Id.r12b, Size.W8
  let r12w   = `X86_64, Id.r12w, Size.W16
  let r12d   = `X86_64, Id.r12d, Size.W32
  let r12    = `X86_64, Id.r12, Size.W64
  let r13b   = `X86_64, Id.r13b, Size.W8
  let r13w   = `X86_64, Id.r13w, Size.W16
  let r13d   = `X86_64, Id.r13d, Size.W32
  let r13    = `X86_64, Id.r13, Size.W64
  let r14b   = `X86_64, Id.r14b, Size.W8
  let r14w   = `X86_64, Id.r14w, Size.W16
  let r14d   = `X86_64, Id.r14d, Size.W32
  let r14    = `X86_64, Id.r14, Size.W64
  let r15b   = `X86_64, Id.r15b, Size.W8
  let r15w   = `X86_64, Id.r15w, Size.W16
  let r15d   = `X86_64, Id.r15d, Size.W32
  let r15    = `X86_64, Id.r15, Size.W64
  let es     = `X86_64, Id.es, Size.W16
  let cs     = `X86_64, Id.cs, Size.W16
  let ss     = `X86_64, Id.ss, Size.W16
  let ds     = `X86_64, Id.ds, Size.W16
  let fs     = `X86_64, Id.fs, Size.W16
  let gs     = `X86_64, Id.gs, Size.W16
  let flags  = `X86_64, Id.eflags, Size.W16
  let eflags = `X86_64, Id.eflags, Size.W32
  let rflags = `X86_64, Id.eflags, Size.W64
  let ip     = `X86_64, Id.ip, Size.W16
  let eip    = `X86_64, Id.eip, Size.W32
  let eiz    = `X86_64, Id.eiz, Size.W32
  let rip    = `X86_64, Id.rip, Size.W64
  let riz    = `X86_64, Id.rip, Size.W64
  let dr0    = `X86_64, Id.dr0, Size.W64
  let dr1    = `X86_64, Id.dr1, Size.W64
  let dr2    = `X86_64, Id.dr2, Size.W64
  let dr3    = `X86_64, Id.dr3, Size.W64
  let dr4    = `X86_64, Id.dr4, Size.W64
  let dr5    = `X86_64, Id.dr5, Size.W64
  let dr6    = `X86_64, Id.dr6, Size.W64
  let dr7    = `X86_64, Id.dr7, Size.W64
  let cr0    = `X86_64, Id.cr0, Size.W64
  let cr1    = `X86_64, Id.cr1, Size.W64
  let cr2    = `X86_64, Id.cr2, Size.W64
  let cr3    = `X86_64, Id.cr3, Size.W64
  let cr4    = `X86_64, Id.cr4, Size.W64

  let read (type rs) e (r : (arch * Id.t * rs Size.t)) : rs =
    let h = Types.handle e in
    let (_, r, s) = r in
    match s with
    | Size.W8 -> Types.reg_read_uint8_ffi h (r :> int)
    | Size.W16 -> Types.reg_read_uint16_ffi h (r :> int)
    | Size.W32 -> Types.reg_read_uint32_ffi h (r :> int)
    | Size.W64 -> Types.reg_read_uint64_ffi h (r :> int)

  let write (type rs) e (r : (arch * Id.t * rs Size.t)) (v : rs) =
    let h = Types.handle e in
    let (_, r, s) = r in
    match s with
    | Size.W8 -> Types.reg_write_uint8_ffi h (r :> int) v
    | Size.W16 -> Types.reg_write_uint16_ffi h (r :> int) v
    | Size.W32 -> Types.reg_write_uint32_ffi h (r :> int) v
    | Size.W64 -> Types.reg_write_uint64_ffi h (r :> int) v

  (* TODO!
  let xmm0 = Id.xmm0, Size.W128
  let ymm0 = Id.ymm0, Size.W256
  let xmm1 = Id.xmm1, Size.W128
  let ymm1 = Id.ymm1, Size.W256
  let xmm2 = Id.xmm2, Size.W128
  let ymm2 = Id.ymm2, Size.W256
  let xmm3 = Id.xmm3, Size.W128
  let ymm3 = Id.ymm3, Size.W256
  let xmm4 = Id.xmm4, Size.W128
  let ymm4 = Id.ymm4, Size.W256
  let xmm5 = Id.xmm5, Size.W128
  let ymm5 = Id.ymm5, Size.W256
  let xmm6 = Id.xmm6, Size.W128
  let ymm6 = Id.ymm6, Size.W256
  let xmm7 = Id.xmm7, Size.W128
  let ymm7 = Id.ymm7, Size.W256
  let xmm8 = Id.xmm8, Size.W128
  let ymm8 = Id.ymm8, Size.W256
  let xmm9 = Id.xmm9, Size.W128
  let ymm9 = Id.ymm9, Size.W256
  let xmm10 = Id.xmm10, Size.W128
  let ymm10 = Id.ymm10, Size.W256
  let xmm11 = Id.xmm11, Size.W128
  let ymm11 = Id.ymm11, Size.W256
  let xmm12 = Id.xmm12, Size.W128
  let ymm12 = Id.ymm12, Size.W256
  let xmm13 = Id.xmm13, Size.W128
  let ymm13 = Id.ymm13, Size.W256
  let xmm14 = Id.xmm14, Size.W128
  let ymm14 = Id.ymm14, Size.W256
  let xmm15 = Id.xmm15, Size.W128
  let ymm15 = Id.ymm15, Size.W256
  let idtr = Id.idtr, X86_MMR (* limit, base *)
  let gdtr = Id.gdtr, X86_MMR (* limit, base *)
  let ldtr = Id.ldtr, X86_MMR (* all fields **)
  let tr = Id.tr, X86_MMR (* all fields *)
  let msr = Id.msr, X86_MSR
  *)
end

let create ?(mode : arch Mode.t option) () =
  let endian = Endian.Little in
  let default_mode = Mode.(mode_64 & little_endian) in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.x86 ~mode in
  engine ~family:`X86 ~endian ~word_size:Size.W64 h
