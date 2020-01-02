module Arm = Arm
module Arm64 = Arm64
module M68k = M68k
module Mips = Mips
module Sparc = Sparc
module X86 = X86

module Const = struct
  module Arch = Uc_const.Arch
  module Err = Uc_const.Err
  module Hook = Uc_const.HookType
  module Mode = Uc_const.Mode
end

exception Unicorn_error of Const.Err.t

type handle

external create_ffi : arch:Const.Arch.t -> mode:int -> handle = "ml_unicorn_create"
external version    : unit -> int * int = "ml_unicorn_version"

module Arch = struct
  type no_insn

  type ('a, 'r, 'i) t =
    | ARM   : ([ `ARM ], Arm.Const.Reg.t, no_insn) t
    | ARM64 : ([ `ARM64 ], Arm64.Const.Reg.t, no_insn) t
    | M68K  : ([ `M68K ], M68k.Const.Reg.t, no_insn) t
    | MIPS  : ([ `MIPS ], Mips.Const.Reg.t, no_insn) t
    | SPARC : ([ `SPARC ], Sparc.Const.Reg.t, no_insn) t
    | X86   : ([ `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) t

  type id = [ `ARM
            | `ARM64
            | `M68K
            | `MIPS
            | `SPARC
            | `X86
            ]
  type any = id
end

type ('a, 'r, 'i) engine = T : ('a, 'r, 'i) Arch.t * handle -> ('a, 'r, 'i) engine

let handle = function T (_, h) -> h

module Mode = struct
  type 'a t =
    | M_LITTLE_ENDIAN : [< Arch.any ] t
    | M_ARM           : [< `ARM ] t
    | M_MODE_16       : [< `X86 ] t
    | M_MODE_32       : [< `MIPS | `SPARC | `X86 ] t
    | M_MODE_64       : [< `MIPS | `SPARC | `X86 ] t
    | M_THUMB         : [< `ARM ] t
    | M_MCLASS        : [< `ARM ] t
    | M_V8            : [< `ARM ] t
    | M_MICRO         : [< `MIPS ] t
    | M_MIPS3         : [< `MIPS ] t
    | M_MIPS32R6      : [< `MIPS ] t
    | M_BIG_ENDIAN    : [< Arch.any] t
    | M_MIPS32        : [< `MIPS ] t
    | M_MIPS64        : [< `MIPS ] t
    | M_SPARC32       : [< `SPARC ] t
    | M_SPARC64       : [< `SPARC ] t
    | M_V9            : [< `SPARC ] t
    | M_PLUS          : 'a t * 'a t -> 'a t

  let little_endian = M_LITTLE_ENDIAN
  let arm = M_ARM
  let mode_16 = M_MODE_16
  let mode_32 = M_MODE_32
  let mode_64 = M_MODE_64
  let thumb = M_THUMB
  let mclass = M_MCLASS
  let v8 = M_V8
  let micro = M_MICRO
  let mips3 = M_MIPS3
  let mips32r6 = M_MIPS32R6
  let big_endian = M_BIG_ENDIAN
  let mips32 = M_MIPS32
  let mips64 = M_MIPS64
  let sparc32 = M_SPARC32
  let sparc64 = M_SPARC64
  let v9 = M_V9
  let (&) v v' = M_PLUS (v, v')

  let rec to_int_mode : 'a. 'a t -> int =
    let aux (type a) (m : a t) : int = match m with
      | M_LITTLE_ENDIAN -> (Const.Mode.little_endian :> int)
      | M_ARM -> (Const.Mode.arm :> int)
      | M_MODE_16 -> (Const.Mode.mode_16 :> int)
      | M_MODE_32 -> (Const.Mode.mode_32 :> int)
      | M_MODE_64 -> (Const.Mode.mode_64 :> int)
      | M_THUMB -> (Const.Mode.thumb :> int)
      | M_MCLASS -> (Const.Mode.mclass :> int)
      | M_V8 -> (Const.Mode.v8 :> int)
      | M_MICRO -> (Const.Mode.micro :> int)
      | M_MIPS3 -> (Const.Mode.mips3 :> int)
      | M_MIPS32R6 -> (Const.Mode.mips32r6 :> int)
      | M_V9 -> (Const.Mode.v9 :> int)
      | M_BIG_ENDIAN -> (Const.Mode.big_endian :> int)
      | M_MIPS32 -> (Const.Mode.mips32 :> int)
      | M_MIPS64 -> (Const.Mode.mips64 :> int)
      | M_SPARC32 -> (Const.Mode.sparc32 :> int)
      | M_SPARC64 -> (Const.Mode.sparc64 :> int)
      | M_PLUS (v, v') -> to_int_mode v land to_int_mode v'
    in aux
end

module Memory = struct
  module Permission = struct
    type t = private int32
  end

  module Access = struct
    (* uc_mem_type *)
    type t = private int
  end

end

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
    | M_HOOK_X86_IN  : (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> 'v -> int32 * 'v) -> ([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback
    | M_HOOK_X86_OUT : (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> int32 -> 'v -> 'v) -> ([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t, 'v) callback

  let code f = M_HOOK_CODE f
  let intr f = M_HOOK_INTR f
  let mem f t = M_HOOK_MEM (f, t)
  let mem_ev f t = M_HOOK_MEMEV (f, t)
  let in_ f = M_HOOK_X86_IN f
  let out f = M_HOOK_X86_OUT f

  external hook_add_code_ffi : handle -> Const.Hook.t -> (('a, 'r, 'i) engine -> int64 -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_intr_ffi : handle -> Const.Hook.t -> (('a, 'r, 'i) engine -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_mem_ffi : handle -> Mem.Valid.t -> (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_memev_ffi : handle -> Mem.Invalid.t -> (('a, 'r, 'i) engine -> Memory.Access.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> 'v -> int64 = "ml_unicorn_hook_add"

  external hook_add_insn_in_ffi : handle -> Const.Hook.t -> (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> 'v -> int32 * 'v) -> 'v -> 'i -> int64 = "ml_unicorn_hook_add_insn"
  external hook_add_insn_out_ffi : handle -> Const.Hook.t -> (([< `X86 ], X86.Const.Reg.t, X86.Const.Insn.t) engine -> int32 -> int -> int32 -> 'v -> 'v) -> 'v -> 'i -> int64 = "ml_unicorn_hook_add_insn"

  external hook_del_ffi : handle -> int64 -> unit = "ml_unicorn_hook_del"

  type ('a, 'r, 'i) handle = ('a, 'r, 'i) engine * int64

  let add (type a) (type r) (type i) (type v) (engine : (a, r, i) engine) (cb : (a, r, i, v) callback) (init : v) : (a, r, i) handle =
    let T (_, e) = engine in
    let h = match cb with
      | M_HOOK_CODE f -> hook_add_code_ffi e Const.Hook.code f init
      | M_HOOK_INTR f -> hook_add_intr_ffi e Const.Hook.intr f init
      | M_HOOK_MEM (f, t) -> hook_add_mem_ffi e t f init
      | M_HOOK_MEMEV (f, t) -> hook_add_memev_ffi e t f init
      | M_HOOK_X86_IN f -> hook_add_insn_in_ffi e Const.Hook.insn f init X86.Const.Insn.in_
      | M_HOOK_X86_OUT f -> hook_add_insn_out_ffi e Const.Hook.insn f init X86.Const.Insn.out
    in (engine, h)

  let remove (type a) (type r) (type i) (e : (a, r, i) engine) (eh : (a, r, i) handle) =
    let (_, h) = eh in
    hook_del_ffi (handle e) h

end

let create (type a) (type r) (type i) ?(mode : a Mode.t option) (arch : (a, r, i) Arch.t) : (a, r, i) engine =
  let arch' = match arch with
    | Arch.ARM -> Const.Arch.arm
    | Arch.ARM64 -> Const.Arch.arm64
    | Arch.M68K -> Const.Arch.m68k
    | Arch.MIPS -> Const.Arch.mips
    | Arch.SPARC -> Const.Arch.sparc
    | Arch.X86 -> Const.Arch.x86
  in
  let mode' = match mode with None -> 0 | Some v -> Mode.to_int_mode v in
  T (arch, create_ffi ~arch:arch' ~mode:mode')
