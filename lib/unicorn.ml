open Stdint
open! Types

module Arm = Arm
module Aarch = Aarch
module M68k = M68k
module Mips64 = Mips.M64
module Mips = Mips.M32
module Sparc64 = Sparc.M32
module Sparc = Sparc.M32
module X86 = X86
module X86_64 = X86_64

module Arch = Types.Arch
module Endian = Types.Endian
module Family = Types.Family
module Mode = Types.Mode

module type S = Types.S

type handle = Types.handle
type ('family, 'endian, 'word) engine = ('family, 'endian, 'word) Types.engine

module Const = struct
  module Arch = Uc_const.Arch
  module Err = Uc_const.Err
  module Hook = Uc_const.HookType
  module Mode = Uc_const.Mode
end

exception Unicorn_error of Const.Err.t
let _ = Callback.register_exception "Unicorn_error" (Unicorn_error Const.Err.ok)

external version    : unit -> int * int = "ml_unicorn_version"

module Memory = struct
  module Permission = struct
    type t = private int32
  end

  module Access = struct
    (* uc_mem_type *)
    type t = private int
  end

end

(*
module Hook = struct
  module Mem = struct
    module Valid = struct
      type t = int

      let read = (Const.Hook.mem_read :> t)
      let write = (Const.Hook.mem_write :> t)
      let fetch = (Const.Hook.mem_fetch :> t)
      let read_after = (Const.Hook.mem_read_after :> t)

      let (&) = (land)
    end

    module Invalid = struct
      type t = int

      let read_unmapped = (Const.Hook.mem_read_unmapped :> t)
      let write_unmapped = (Const.Hook.mem_write_unmapped :> t)
      let fetch_unmapped = (Const.Hook.mem_fetch_unmapped :> t)

      let read_prot = (Const.Hook.mem_read_prot :> t)
      let write_prot = (Const.Hook.mem_write_prot :> t)
      let fetch_prot = (Const.Hook.mem_fetch_prot :> t)

      let (&) = (land)
    end
  end

  type 'v cont = Continue of 'v
               | Stop of 'v

  type ('a, 'r, 'i, 'v) callback =
    | M_HOOK_CODE    : (('a, 'r, 'i) engine -> int64 -> int32 -> 'v -> 'v) -> ('a, 'r, 'i, 'v) callback
    | M_HOOK_INTR    : (('a, 'r, 'i) engine -> int32 -> 'v -> 'v) -> ('a, 'r, 'i, 'v) callback
    | M_HOOK_MEM     : (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) * Mem.Valid.t -> ('a, 'r, 'i, 'v) callback
    | M_HOOK_MEMEV   : (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int64 -> 'v -> 'v cont) * Mem.Invalid.t -> ('a, 'r, 'i, 'v) callback
    | M_HOOK_X86_IN  : (([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> 'v -> int32 * 'v) -> ([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback
    | M_HOOK_X86_OUT : (([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> int32 -> 'v -> 'v) -> ([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback

  let code f = M_HOOK_CODE f
  let intr f = M_HOOK_INTR f
  let mem f t = M_HOOK_MEM (f, t)
  let mem_ev f t = M_HOOK_MEMEV (f, t)
  let in_ f = M_HOOK_X86_IN f
  let out f = M_HOOK_X86_OUT f

  external hook_add_code_ffi : handle -> Const.Hook.t -> (handle -> int64 -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_intr_ffi : handle -> Const.Hook.t -> (handle -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_mem_ffi : handle -> Mem.Valid.t -> (handle -> Memory.Access.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_memev_ffi : handle -> Mem.Invalid.t -> (handle -> Memory.Access.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> 'v -> int64 = "ml_unicorn_hook_add"

  external hook_add_insn_in_ffi : handle -> Const.Hook.t -> (handle -> int32 -> int -> 'v -> int32 * 'v) -> 'v -> X86.Const.Insn.t -> int64 = "ml_unicorn_hook_add_insn"
  external hook_add_insn_out_ffi : handle -> Const.Hook.t -> (handle -> int32 -> int -> int32 -> 'v -> 'v) -> 'v -> X86.Const.Insn.t -> int64 = "ml_unicorn_hook_add_insn"

  external hook_del_ffi : handle -> int64 -> unit = "ml_unicorn_hook_del"

  type ('a, 'r, 'i) handle = ('a, 'r, 'i) engine * int64

  let add (type a) (type r) (type i) (type v) (e : (a, r, i) engine) (cb : (a, r, i, v) callback) (init : v) : (a, r, i) handle =
    let T (a, eh) = e in
    let mk_f f e' = f (T (a, e')) in
    let h = match cb with
      | M_HOOK_CODE f -> hook_add_code_ffi eh Const.Hook.code (mk_f f) init
      | M_HOOK_INTR f -> hook_add_intr_ffi eh Const.Hook.intr (mk_f f) init
      | M_HOOK_MEM (f, t) -> hook_add_mem_ffi eh t (mk_f f) init
      | M_HOOK_MEMEV (f, t) -> hook_add_memev_ffi eh t (mk_f f) init
      | M_HOOK_X86_IN f -> hook_add_insn_in_ffi eh Const.Hook.insn (mk_f f) init X86.Const.Insn.in_
      | M_HOOK_X86_OUT f -> hook_add_insn_out_ffi eh Const.Hook.insn (mk_f f) init X86.Const.Insn.out
    in (e, h)

  let remove (type a) (type r) (type i) (e : (a, r, i) engine) (eh : (a, r, i) handle) =
    let (_, h) = eh in
    hook_del_ffi (handle e) h

end

let create (type a) (type r) (type i) ?(mode : a Mode.t option) (arch : (a, r, i) Arch.t) : (a, r, i) engine =
  let arch', mode' = match arch with
    | Arch.ARM -> Const.Arch.arm, mode
    | Arch.AARCH64 -> Const.Arch.arm64, mode
    | Arch.M68K -> Const.Arch.m68k, mode
    | Arch.MIPS -> Const.Arch.mips, mode
    | Arch.SPARC -> Const.Arch.sparc, mode
    | Arch.X86 -> Const.Arch.x86, mode
    | Arch.X86_64 -> Const.Arch.x86, match mode with None -> Some Mode.mode_64 | Some v -> Some (Mode.(v & mode_64))
  in
  let mode'' = match mode' with None -> 0 | Some v -> Mode.to_int_mode v in
  T (arch, create_ffi ~arch:arch' ~mode:mode'')

module Register = struct
  let write (type a) (type rt) (type rs) (type i) (_e : (a, rt, i) engine) (_r : (rt, rs) reg) (_v : rs) : unit =
    failwith "unimplemented"

  let read (type a) (type rt) (type rs) (type i) (_e : (a, rt, i) engine) (_r : (rt, rs) reg) : rs =
    failwith "unimplemented"
end
   *)

let create (type a) (type f) (type w) (type e)
    ?(mode : a mode option)
    (module M : S with type arch = a
                   and type family = f
                   and type endian = e
                   and type word = w) =
  M.create ?mode ()

let into_m16 (type f) (type w) (type e)
    (module M : Into16 with type endian = e
                        and type family = f
                        and type word = w) =
  M.into_m16

let into_m32 (type f) (type w) (type e)
    (module M : Into32 with type endian = e
                        and type family = f
                        and type word = w) =
  M.into_m32

let into_m64 (type f) (type w) (type e)
    (module M : Into64 with type endian = e
                        and type family = f
                        and type word = w) =
  M.into_m64
