open! Types

module Arm     : module type of Arm
module Aarch   : module type of Aarch
module M68k    : module type of M68k
module Mips64  : module type of Mips.M64
module Mips    : module type of Mips.M32
module Sparc64 : module type of Sparc.M32
module Sparc   : module type of Sparc.M32
module X86     : module type of X86
module X86_64  : module type of X86_64

module Arch    : module type of Types.Arch
module Endian  : module type of Types.Endian
module Family  : module type of Types.Family
module Mode    : module type of Types.Mode

module Const   : sig
  module Err   : sig
    type t

    val arch            : t
    val arg             : t
    val exception_      : t
    val fetch_prot      : t
    val fetch_unaligned : t
    val fetch_unmapped  : t
    val handle          : t
    val hook            : t
    val hook_exist      : t
    val map             : t
    val mode            : t
    val nomem           : t
    val read_prot       : t
    val read_unaligned  : t
    val read_unmapped   : t
    val resource        : t
    val version         : t
    val write_prot      : t
    val write_unaligned : t
    val write_unmapped  : t
  end
end

module type S = Types.S

exception Unicorn_error of Const.Err.t

type ('family, 'endian, 'word) engine

val create  : ?mode:'a mode -> (module S with type arch = 'a
                                          and type endian = 'e
                                          and type family = 'f
                                          and type word = 'w) -> ('f, 'e, 'w) engine
val version : unit -> int * int
