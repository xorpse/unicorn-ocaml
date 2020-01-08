open Stdint
open Types

type arch   = Arch.x86
type family = Family.x86
type word   = uint32

module Insn = X86_const.Insn
module Reg = struct
  module Id = X86_const.Reg

  let al     = `X86, Id.al, Size.W8
  let ax     = `X86, Id.ax, Size.W16
  let eax    = `X86, Id.eax, Size.W32
  let ah     = `X86, Id.ah, Size.W8
  let cl     = `X86, Id.cl, Size.W8
  let cx     = `X86, Id.cx, Size.W16
  let ecx    = `X86, Id.ecx, Size.W32
  let ch     = `X86, Id.ch, Size.W8
  let dl     = `X86, Id.dl, Size.W8
  let dx     = `X86, Id.dx, Size.W16
  let edx    = `X86, Id.edx, Size.W32
  let dh     = `X86, Id.dh, Size.W8
  let bl     = `X86, Id.bl, Size.W8
  let bx     = `X86, Id.bx, Size.W16
  let ebx    = `X86, Id.ebx, Size.W32
  let bh     = `X86, Id.bh, Size.W8
  let spl    = `X86, Id.spl, Size.W8
  let sp     = `X86, Id.sp, Size.W16
  let esp    = `X86, Id.esp, Size.W32
  let bpl    = `X86, Id.bpl, Size.W8
  let bp     = `X86, Id.bp, Size.W16
  let ebp    = `X86, Id.ebp, Size.W32
  let sil    = `X86, Id.sil, Size.W8
  let si     = `X86, Id.si, Size.W16
  let esi    = `X86, Id.esi, Size.W32
  let dil    = `X86, Id.dil, Size.W8
  let di     = `X86, Id.di, Size.W16
  let edi    = `X86, Id.edi, Size.W32
  let es     = `X86, Id.es, Size.W16
  let cs     = `X86, Id.cs, Size.W16
  let ss     = `X86, Id.ss, Size.W16
  let ds     = `X86, Id.ds, Size.W16
  let fs     = `X86, Id.fs, Size.W16
  let gs     = `X86, Id.gs, Size.W16
  let flags  = `X86, Id.eflags, Size.W16
  let eflags = `X86, Id.eflags, Size.W32
  let ip     = `X86, Id.ip, Size.W16
  let eip    = `X86, Id.eip, Size.W32
  let eiz    = `X86, Id.eiz, Size.W32
  let dr0    = `X86, Id.dr0, Size.W32
  let dr1    = `X86, Id.dr1, Size.W32
  let dr2    = `X86, Id.dr2, Size.W32
  let dr3    = `X86, Id.dr3, Size.W32
  let dr4    = `X86, Id.dr4, Size.W32
  let dr5    = `X86, Id.dr5, Size.W32
  let dr6    = `X86, Id.dr6, Size.W32
  let dr7    = `X86, Id.dr7, Size.W32
  let cr0    = `X86, Id.cr0, Size.W32
  let cr1    = `X86, Id.cr1, Size.W32
  let cr2    = `X86, Id.cr2, Size.W32
  let cr3    = `X86, Id.cr3, Size.W32
  let cr4    = `X86, Id.cr4, Size.W32

(* TODO
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

type mmr = {
  selector : int option;
  base     : int64;
  limit    : int32;
  flags    : int32 option;
}

type msr = {
  rid   : int32;
  value : int64;
}

let create ?(mode : arch Mode.t option) () =
  let endian = Endian.Little in
  let default_mode = Mode.(mode_32 & little_endian) in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.x86 ~mode in
  engine ~family:`X86 ~endian ~word_size:Size.W32 h
