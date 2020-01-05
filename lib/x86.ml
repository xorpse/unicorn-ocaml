open Stdint
open Types

type arch   = Arch.x86
type family = Family.x86
type word   = uint32
type endian = Endian.little

module Insn = X86_const.Insn
module Reg = struct
  module Id = X86_const.Reg

  let al     = Id.al, Word.W8
  let ax     = Id.ax, Word.W16
  let eax    = Id.eax, Word.W32
  let ah     = Id.ah, Word.W8
  let cl     = Id.cl, Word.W8
  let cx     = Id.cx, Word.W16
  let ecx    = Id.ecx, Word.W32
  let ch     = Id.ch, Word.W8
  let dl     = Id.dl, Word.W8
  let dx     = Id.dx, Word.W16
  let edx    = Id.edx, Word.W32
  let dh     = Id.dh, Word.W8
  let bl     = Id.bl, Word.W8
  let bx     = Id.bx, Word.W16
  let ebx    = Id.ebx, Word.W32
  let bh     = Id.bh, Word.W8
  let spl    = Id.spl, Word.W8
  let sp     = Id.sp, Word.W16
  let esp    = Id.esp, Word.W32
  let bpl    = Id.bpl, Word.W8
  let bp     = Id.bp, Word.W16
  let ebp    = Id.ebp, Word.W32
  let sil    = Id.sil, Word.W8
  let si     = Id.si, Word.W16
  let esi    = Id.esi, Word.W32
  let dil    = Id.dil, Word.W8
  let di     = Id.di, Word.W16
  let edi    = Id.edi, Word.W32
  let es     = Id.es, Word.W16
  let cs     = Id.cs, Word.W16
  let ss     = Id.ss, Word.W16
  let ds     = Id.ds, Word.W16
  let fs     = Id.fs, Word.W16
  let gs     = Id.gs, Word.W16
  let flags  = Id.eflags, Word.W16
  let eflags = Id.eflags, Word.W32
  let ip     = Id.ip, Word.W16
  let eip    = Id.eip, Word.W32
  let eiz    = Id.eiz, Word.W32
  let dr0    = Id.dr0, Word.W32
  let dr1    = Id.dr1, Word.W32
  let dr2    = Id.dr2, Word.W32
  let dr3    = Id.dr3, Word.W32
  let dr4    = Id.dr4, Word.W32
  let dr5    = Id.dr5, Word.W32
  let dr6    = Id.dr6, Word.W32
  let dr7    = Id.dr7, Word.W32
  let cr0    = Id.cr0, Word.W32
  let cr1    = Id.cr1, Word.W32
  let cr2    = Id.cr2, Word.W32
  let cr3    = Id.cr3, Word.W32
  let cr4    = Id.cr4, Word.W32

(* TODO
  let xmm0 = Id.xmm0, Word.W128
  let ymm0 = Id.ymm0, Word.W256
  let xmm1 = Id.xmm1, Word.W128
  let ymm1 = Id.ymm1, Word.W256
  let xmm2 = Id.xmm2, Word.W128
  let ymm2 = Id.ymm2, Word.W256
  let xmm3 = Id.xmm3, Word.W128
  let ymm3 = Id.ymm3, Word.W256
  let xmm4 = Id.xmm4, Word.W128
  let ymm4 = Id.ymm4, Word.W256
  let xmm5 = Id.xmm5, Word.W128
  let ymm5 = Id.ymm5, Word.W256
  let xmm6 = Id.xmm6, Word.W128
  let ymm6 = Id.ymm6, Word.W256
  let xmm7 = Id.xmm7, Word.W128
  let ymm7 = Id.ymm7, Word.W256
  let xmm8 = Id.xmm8, Word.W128
  let ymm8 = Id.ymm8, Word.W256
  let xmm9 = Id.xmm9, Word.W128
  let ymm9 = Id.ymm9, Word.W256
  let xmm10 = Id.xmm10, Word.W128
  let ymm10 = Id.ymm10, Word.W256
  let xmm11 = Id.xmm11, Word.W128
  let ymm11 = Id.ymm11, Word.W256
  let xmm12 = Id.xmm12, Word.W128
  let ymm12 = Id.ymm12, Word.W256
  let xmm13 = Id.xmm13, Word.W128
  let ymm13 = Id.ymm13, Word.W256
  let xmm14 = Id.xmm14, Word.W128
  let ymm14 = Id.ymm14, Word.W256
  let xmm15 = Id.xmm15, Word.W128
  let ymm15 = Id.ymm15, Word.W256
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

let create ?(mode : arch mode option) () =
  let default_mode = Mode.((Uc_const.Mode.mode_32 :> arch mode) &
                           (Endian.Little.mode :> arch mode))
  in
  let mode = match mode with None -> default_mode | Some m -> Mode.(m & default_mode) in
  let h = create_ffi ~arch:Uc_const.Arch.x86 ~mode in
  engine ~family:`X86 ~endian:Endian.LE ~word_size:Word.W32 h

let into_m16 e = { e with word_size = Word.W16 }
let into_m64 e = { e with word_size = Word.W64 }
