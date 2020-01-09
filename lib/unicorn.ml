open Stdint
open Types

module Arm     = Arm
module Aarch64 = Aarch64
module M68k    = M68k
module Mips64  = Mips.M64
module Mips    = Mips.M32
module Sparc64 = Sparc.M32
module Sparc   = Sparc.M32
module X86     = X86
module X86_64  = X86_64

module Types   = Types
module Arch    = Types.Arch
module Endian  = Types.Endian
module Family  = Types.Family
module Mode    = Types.Mode


module Const = struct
  module Arch = Uc_const.Arch
  module Err  = Uc_const.Err
  module Hook = Uc_const.HookType
  module Mode = Uc_const.Mode
end

exception Unicorn_error of Const.Err.t
let _ = Callback.register_exception "Unicorn_error" (Unicorn_error Const.Err.ok)

external version : unit -> int * int = "ml_unicorn_version"
external start_ffi : handle -> uint64 -> uint64 -> uint64 -> int -> unit = "ml_unicorn_start"
external stop_ffi : handle -> unit = "ml_unicorn_stop"

module Memory = struct
  module Permission = struct
    type t = private int32
  end

  external read_uint8_ffi : handle -> uint8 -> uint8 = "ml_unicorn_read_uint8"
  external read_uint16_ffi : handle -> uint16 -> uint16 = "ml_unicorn_read_uint16"
  external read_uint32_ffi : handle -> uint32 -> uint32 = "ml_unicorn_read_uint32"
  external read_uint64_ffi : handle -> uint64 -> uint64 = "ml_unicorn_read_uint64"

  (* TODO: handle endian? *)
  let read_word (type f) (type w)
      (module M : Types.S with type family = f
                           and type word = w)
      (e : (f, w) engine) (a : w) : w =
    let h = Types.handle e in
    match Types.word_size e with
      | Size.W8 -> read_uint8_ffi h a
      | Size.W16 -> read_uint16_ffi h a
      | Size.W32 -> read_uint32_ffi h a
      | Size.W64 -> read_uint64_ffi h a

  external write_uint8_ffi : handle -> uint8 -> uint8 -> unit = "ml_unicorn_write_uint8"
  external write_uint16_ffi : handle -> uint16 -> uint16 -> unit = "ml_unicorn_write_uint16"
  external write_uint32_ffi : handle -> uint32 -> uint32 -> unit = "ml_unicorn_write_uint32"
  external write_uint64_ffi : handle -> uint64 -> uint64 -> unit = "ml_unicorn_write_uint64"

  let write_word (type f) (type w)
      (module M : Types.S with type family = f
                           and type word = w)
      (e : (f, w) engine) (a : w) (v : w) : unit =
    let h = Types.handle e in
    match Types.word_size e with
      | Size.W8 -> write_uint8_ffi h a v
      | Size.W16 -> write_uint16_ffi h a v
      | Size.W32 -> write_uint32_ffi h a v
      | Size.W64 -> write_uint64_ffi h a v

  external read_bytes_ffi : handle -> uint64 -> int -> bytes = "ml_unicorn_read_bytes"

  let read_bytes (type f) (type w)
      (e : (f, w) engine) (a : w) (n : int) : bytes =
    let h = Types.handle e in
    let a = match Types.word_size e with
      | Size.W8 -> Uint8.to_uint64 a
      | Size.W16 -> Uint16.to_uint64 a
      | Size.W32 -> Uint32.to_uint64 a
      | Size.W64 -> a
    in
    read_bytes_ffi h a n

  external write_bytes_ffi : handle -> uint64 -> bytes -> unit = "ml_unicorn_write_bytes"

  let write_bytes (type f) (type w)
      (e : (f, w) engine) (a : w) (buf : bytes) : unit =
    let h = Types.handle e in
    let a = match Types.word_size e with
      | Size.W8 -> Uint8.to_uint64 a
      | Size.W16 -> Uint16.to_uint64 a
      | Size.W32 -> Uint32.to_uint64 a
      | Size.W64 -> a
    in
    write_bytes_ffi h a buf

  external map_ffi : handle -> uint64 -> int -> int32 -> unit = "ml_unicorn_map"

  let map (type f) (type w)
      (e : (f, w) engine) (a : w) (n : int) (p : Permission.t) : unit =
    let h = Types.handle e in
    let a = match Types.word_size e with
      | Size.W8 -> Uint8.to_uint64 a
      | Size.W16 -> Uint16.to_uint64 a
      | Size.W32 -> Uint32.to_uint64 a
      | Size.W64 -> a
    in
    map_ffi h a n (p :> int32)

  let map_bytes (type f) (type w)
      (e : (f, w) engine) (a : w) (buf : bytes) (p : Permission.t) : unit =
    map e a (Bytes.length buf) p; write_bytes e a buf

  external unmap_ffi : handle -> uint64 -> int -> unit = "ml_unicorn_unmap"

  let unmap (type f) (type w)
      (e : (f, w) engine) (a : w) (n : int) : unit =
    let h = Types.handle e in
    let a = match Types.word_size e with
      | Size.W8 -> Uint8.to_uint64 a
      | Size.W16 -> Uint16.to_uint64 a
      | Size.W32 -> Uint32.to_uint64 a
      | Size.W64 -> a
    in
    unmap_ffi h a n

  external protect_ffi : handle -> uint64 -> int -> int32 -> unit = "ml_unicorn_protect"

  let protect (type f) (type w)
      (e : (f, w) engine) (a : w) (n : int) (p : Permission.t) : unit =
    let h = Types.handle e in
    let a = match Types.word_size e with
      | Size.W8 -> Uint8.to_uint64 a
      | Size.W16 -> Uint16.to_uint64 a
      | Size.W32 -> Uint32.to_uint64 a
      | Size.W64 -> a
    in
    protect_ffi h a n (p :> int32)
end

module Hook = struct
  module Memory = struct
    module Access = struct
      module Valid = struct
        type t = int

        let read = (Const.Hook.mem_read :> t)
        let write = (Const.Hook.mem_write :> t)
        let fetch = (Const.Hook.mem_fetch :> t)
        let read_after = (Const.Hook.mem_read_after :> t)

        let (&) = (lor)
      end

      module Invalid = struct
        type t = int

        let read_unmapped = (Const.Hook.mem_read_unmapped :> t)
        let write_unmapped = (Const.Hook.mem_write_unmapped :> t)
        let fetch_unmapped = (Const.Hook.mem_fetch_unmapped :> t)

        let read_prot = (Const.Hook.mem_read_prot :> t)
        let write_prot = (Const.Hook.mem_write_prot :> t)
        let fetch_prot = (Const.Hook.mem_fetch_prot :> t)

        let (&) = (lor)
      end
    end
  end

  type 'v cont = Continue of 'v
               | Stop of 'v

  type ('f, 'w, 'v) callback =
    | M_HOOK_CODE    : (('f, 'w) engine -> int64 -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
    | M_HOOK_INTR    : (('f, 'w) engine -> int32 -> 'v -> 'v) -> ('f, 'w, 'v) callback
    | M_HOOK_MEM     : (('f, 'w) engine -> Memory.Access.Valid.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) * Memory.Access.Valid.t -> ('f, 'w, 'v) callback
    | M_HOOK_MEMEV   : (('f, 'w) engine -> Memory.Access.Valid.t -> int64 -> int -> int64 -> 'v -> 'v cont) * Memory.Access.Invalid.t -> ('f, 'w, 'v) callback
(*
    | M_HOOK_X86_IN  : ((Family.x86, 'w) engine -> int32 -> int -> 'v -> int32 * 'v) -> (Family.x86, 'w, 'v) callback
    | M_HOOK_X86_OUT : ((Family.x86, 'w) engine -> int32 -> int -> int32 -> 'v -> 'v) -> (Family.x86, 'w, 'v) callback
 *)

  let code f = M_HOOK_CODE f
  let intr f = M_HOOK_INTR f
  let mem f t = M_HOOK_MEM (f, t)
  let mem_ev f t = M_HOOK_MEMEV (f, t)
(*
  let in_ f = M_HOOK_X86_IN f
  let out f = M_HOOK_X86_OUT f
*)

  external hook_add_code_ffi : handle -> Const.Hook.t -> (handle -> int64 -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_intr_ffi : handle -> Const.Hook.t -> (handle -> int32 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_mem_ffi : handle -> Memory.Access.Valid.t -> (handle -> Memory.Access.Valid.t -> int64 -> int -> int32 -> int64 -> 'v -> 'v) -> 'v -> int64 = "ml_unicorn_hook_add"
  external hook_add_memev_ffi : handle -> Memory.Access.Invalid.t -> (handle -> Memory.Access.Invalid.t -> int64 -> int -> int64 -> 'v -> 'v cont) -> 'v -> int64 = "ml_unicorn_hook_add"

  (*
  external hook_add_insn_in_ffi : handle -> Const.Hook.t -> (handle -> int32 -> int -> 'v -> int32 * 'v) -> 'v -> X86.Const.Insn.t -> int64 = "ml_unicorn_hook_add_insn"
  external hook_add_insn_out_ffi : handle -> Const.Hook.t -> (handle -> int32 -> int -> int32 -> 'v -> 'v) -> 'v -> X86.Const.Insn.t -> int64 = "ml_unicorn_hook_add_insn"
     *)

  external hook_del_ffi : handle -> int64 -> unit = "ml_unicorn_hook_del"

  type ('f, 'w) handle = ('f, 'w) engine * int64

  let add (type f) (type w) (type v) (e : (f, w) engine) (cb : (f, w, v) callback) (init : v) : (f, w) handle =
    let family = Types.family e in
    let endian = Types.endian e in
    let word_size = Types.word_size e in
    let h = Types.handle e in
    let mk_f f h' = f (engine ~family ~endian ~word_size h') in
    let h = match cb with
      | M_HOOK_CODE f -> hook_add_code_ffi h Const.Hook.code (mk_f f) init
      | M_HOOK_INTR f -> hook_add_intr_ffi h Const.Hook.intr (mk_f f) init
      | M_HOOK_MEM (f, t) -> hook_add_mem_ffi h t (mk_f f) init
      | M_HOOK_MEMEV (f, t) -> hook_add_memev_ffi h t (mk_f f) init
                                 (*
      | M_HOOK_X86_IN f -> hook_add_insn_in_ffi eh Const.Hook.insn (mk_f f) init X86.Const.Insn.in_
      | M_HOOK_X86_OUT f -> hook_add_insn_out_ffi eh Const.Hook.insn (mk_f f) init X86.Const.Insn.out
                                    *)
    in (e, h)

  let remove (type f) (type w) (e : (f, w) engine) (eh : (f, w) handle) : unit =
    let (eh, h) = eh in
    assert (eh = e);
    hook_del_ffi (handle eh) h
end

module Register = struct
  let write (type a) (type f) (type w) (type r) (type rs)
      (module M : Types.S_Reg with type arch = a
                               and type family = f
                               and type word = w
                               and type Reg.Id.t = r)
      (e : (f, w) engine) (r : (a, r, rs) reg) (v : rs) : unit =
    M.Reg.write e r v

  let read (type a) (type f) (type w) (type r) (type rs)
      (module M : S_Reg with type arch = a
                         and type family = f
                         and type word = w
                         and type Reg.Id.t = r)
    (e : (f, w) engine) (r : (a, r, rs) reg) : rs =
    M.Reg.read e r
end

type timeout = uint64

let no_timeout = Uint64.zero
let timeout t = if Uint64.(t = zero) then invalid_arg "timeout must be greater than 0" else t

type instruction_count = int

let no_limit = 0
let limit t = if t = 0 then invalid_arg "instruction count limit must be greater than 0" else t

let start (type f) (type w) ?(timeout : timeout = no_timeout) ?(limit : instruction_count = no_limit) ~(address : w) ~(until : w) (e : (f, w) engine) =
  let a, u = match Types.word_size e with
    | Size.W8 -> Uint8.(to_uint64 address, to_uint64 until)
    | Size.W16 -> Uint16.(to_uint64 address, to_uint64 until)
    | Size.W32 -> Uint32.(to_uint64 address, to_uint64 until)
    | Size.W64 -> Uint64.(to_uint64 address, to_uint64 until)
  in
  start_ffi Types.(handle e) a u timeout limit

let stop (type f) (type w) (e : (f, w) engine) : unit =
  stop_ffi (handle e)
