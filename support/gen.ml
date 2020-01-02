let includes = [
  "unicorn.h"; "arm.h"; "arm64.h"; "m68k.h";
  "mips.h"; "sparc.h"; "x86.h";
]

let template = Hashtbl.of_seq @@ List.to_seq [
    (* filename templates *)
    "ml_suffix",    "_const.ml";
    "c_suffix",     "_const_stubs.c";
    "h_suffix",     "_const_stubs.h";
    (* header to module mapping *)
    "unicorn.h",    "uc";
    "arm.h",        "arm";
    "arm64.h",      "arm64";
    "m68k.h",       "m68k";
    "mips.h",       "mips";
    "sparc.h",      "sparc";
    "x86.h",        "x86";
  ]

(* if not in [variant_mapping] then in variant;
   for vals if in [variant_mapping] then map to value
*)

let variant_mapping = Hashtbl.of_seq @@ List.to_seq [
    "uc_mode", Hashtbl.of_seq @@ List.to_seq [
      "ARM",           "LITTLE_ENDIAN";
      "MIPS32",        "MODE_32";
      "MIPS64",        "MODE_64";
      "MICRO",         "THUMB";
      "V9",            "THUMB";
      "MIPS3",         "MCLASS";
      "MIPS32R6",      "V8";
      "PPC32",         "MODE_32";
      "PPC64",         "MODE_64";
      "QPX",           "THUMB";
      "SPARC32",       "MODE_32";
      "SPARC64",       "MODE_64";
      "V9",            "THUMB";
    ];
    "uc_arm_reg", Hashtbl.of_seq @@ List.to_seq [
      "SP",  "R13";
      "LR",  "R14";
      "PC",  "R15";
      "SB",  "R9";
      "SL",  "R10";
      "FP",  "R11";
      "IP",  "R12";
    ];
    "uc_arm64_reg", Hashtbl.of_seq @@ List.to_seq [
      "IP0", "X16";
      "IP1", "X17";
      "FP",  "X29";
      "LR",  "X30";
    ];
    "uc_mips_reg", Hashtbl.of_seq @@ List.to_seq [
      "ZERO", "REG_0";
      "AT",   "REG_1";
      "V0",   "REG_2";
      "V1",   "REG_3";
      "A0",   "REG_4";
      "A1",   "REG_5";
      "A2",   "REG_6";
      "A3",   "REG_7";
      "T0",   "REG_8";
      "T1",   "REG_9";
      "T2",   "REG_10";
      "T3",   "REG_11";
      "T4",   "REG_12";
      "T5",   "REG_13";
      "T6",   "REG_14";
      "T7",   "REG_15";
      "S0",   "REG_16";
      "S1",   "REG_17";
      "S2",   "REG_18";
      "S3",   "REG_19";
      "S4",   "REG_20";
      "S5",   "REG_21";
      "S6",   "REG_22";
      "S7",   "REG_23";
      "T8",   "REG_24";
      "T9",   "REG_25";
      "K0",   "REG_26";
      "K1",   "REG_27";
      "GP",   "REG_28";
      "SP",   "REG_29";
      "S8",   "REG_30";
      "FP",   "REG_30";
      "RA",   "REG_31";
      "HI0",  "AC0";
      "HI1",  "AC1";
      "HI2",  "AC2";
      "HI3",  "AC3";
      "LO0",  "AC0";
      "LO1",  "AC1";
      "LO2",  "AC2";
      "LO3",  "AC3";
    ];
    "uc_sparc_reg", Hashtbl.of_seq @@ List.to_seq [
      "FP", "I6";
      "SP", "O6";
    ];
    "uc_x86_prefix", Hashtbl.of_seq @@ List.to_seq [
      "REPE", "REP";
    ];
  ]

module Syms = struct
  module T = struct
    type t = string * string

    let compare x y = compare (fst x) (fst y)
  end

  include T
  module Set = Set.Make(T)
  module Map = Map.Make(T)
end

(* Utility functions *)

let val_int x =
  succ @@ x lsl 1

let caml_hash_variant s =
  let acc = ref 0 in
  for i = 0 to String.length s - 1 do
    acc := 223 * !acc + Char.code s.[i]
  done;
  acc := !acc land (1 lsl 31 - 1);
  val_int @@ if !acc > 0x3fffffff then !acc - (1 lsl 31) else !acc

(* Standard library extras to avoid extra build dependencies *)

module Option = struct
  let return v = Some v
  let (>>=) v f = match v with None -> None | Some v' -> f v'
  let (>>|) v f = v >>= (fun v' -> Some (f v'))

  let value ~default = function None -> default | Some v -> v
  let value_exn = function None -> failwith "no value" | Some v -> v
end

module List = struct
  include ListLabels

  let rec drop n xs =
    if n > 0 then match xs with
      | [] -> xs
      | _ :: xs' -> drop (pred n) xs'
    else
      xs

  let hd_ex = hd
  let hd = function [] -> None | x :: _ -> Some x

  let rec last_exn = function
    | [] -> failwith "empty list"
    | [x] -> x
    | _ :: xs -> last_exn xs
end

module String = struct
  include StringLabels

  let starts_with s v =
    Str.string_match (Str.regexp_string s) v 0

  let ends_with s v =
    let slen = String.length s in
    let vlen = String.length v in
    if slen > vlen then false
    else Str.string_match (Str.regexp_string s) v (vlen - slen)
end

(* Other utility functions *)

let is_digit = function '0' .. '9' -> true | _ -> false

(* Trim whitespace, parenthesis, comments *)
let trim =
  let aux_l = Str.split (Str.regexp "^[ \r\n\t(]+") in
  let aux_r = Str.split (Str.regexp "[ \r\n\t)]+$") in
  let aux_c = Str.split (Str.regexp "//.*$") in
  let open Option in
  fun v ->
    value ~default: "" (
      aux_l v |> List.hd >>= fun v ->
      aux_c v |> List.hd >>= fun v ->
      aux_r v |> List.hd
    )

(* Split on whitespace and parenthesis *)
let split_wsbr =
  let aux = Str.split (Str.regexp "[ \t\r\n()]+") in
  fun v -> aux v

let with_file_in p ~f =
  let op = open_in p in
  let v = f op in
  ignore @@ close_in op;
  v

(* Open file for given arch [suffix_key] *)
let open_out_file prefix suffix_key =
  open_out @@ prefix ^ Hashtbl.find template suffix_key

(* Get type name from enum definition *)
let get_typename line =
  if String.starts_with "typedef" line then (
    match split_wsbr line with
    | _ :: "enum" :: typ :: _ -> Some typ
    | _ -> None
  ) else if String.starts_with "enum" line then (
    match split_wsbr line with
    | _ :: typ :: _ -> Some typ
    | _ -> None
  ) else
    None

(* Update syms and mapper based on current line part [t] *)
let get_syms ~mapper ~typ ~prefix syms t =
  if t = "" || String.starts_with "//" t then syms
  else match split_wsbr t with
    | _ :: sep :: _ when sep <> "=" -> syms
    | name :: _ when String.starts_with (String.uppercase_ascii prefix) name -> begin
        let sym = String.split_on_char ~sep:'_' name |>
                  (if String.lowercase_ascii prefix = "uc" then List.drop 2 else List.drop 3) |>
                  String.concat ~sep:"_" |>
                  String.uppercase_ascii
        in

        let sym = if is_digit sym.[0] then
            let pr = String.split_on_char ~sep:'_' typ |>
                     List.last_exn |>
                     String.uppercase_ascii
            in
            pr ^ "_" ^ sym
          else
            sym
        in

      (*
        let sym = value ~default:sym @@ (Hashtbl.find_opt variant_mapping typ >>= fun tm ->
          Hashtbl.find_opt tm sym)
        in
      *)

        let sym_name = "UNICORN_ML_SYM_" ^ (String.uppercase_ascii sym) in
        let lhs = trim name in

        if String.ends_with "_ENDING" lhs ||
           String.ends_with "_MAX" lhs ||
           String.ends_with "_INVALID" lhs
        then
          syms
        else match Hashtbl.find_opt mapper typ with
          | None ->
            Syms.Map.singleton (sym_name, sym) lhs |> Hashtbl.add mapper typ;
            Syms.Set.add (sym_name, sym) syms
          | Some mt ->
            if Syms.Map.mem (sym_name, sym) mt then
              syms
            else (
              Syms.Map.add (sym_name, sym) lhs mt |> Hashtbl.replace mapper typ;
              Syms.Set.add (sym_name, sym) syms
            )
      end
    | _ -> syms

(* Perform the .ml/.c/.h file generation *)
let generate () =
  let syms = List.fold_left includes ~init:Syms.Set.empty ~f:(fun syms target ->
      let prefix  = Hashtbl.find template target in
      let aprefix = String.uppercase_ascii @@ if String.starts_with "uc" prefix then prefix else "uc_" ^ prefix in
      let ml_file = open_out_file prefix "ml_suffix" in
      let c_file  = open_out_file prefix "c_suffix" in
      let h_file  = open_out_file prefix "h_suffix" in

      let mapper = Hashtbl.create 100 in

      let syms' = with_file_in ("/usr/include/unicorn/" ^ target) ~f:(fun p ->
          let rec process_lines ?typ syms =
            match try Some (trim @@ input_line p) with End_of_file -> None with
            | None -> syms
            | Some line ->
              if line = "" || String.starts_with "//" line then
                process_lines ?typ syms
              else
                match get_typename line with
                | Some typ -> process_lines ~typ syms
                | _ ->
                  if not @@ String.starts_with (String.uppercase_ascii aprefix) line then
                    process_lines ?typ syms
                  else
                    List.fold_left (List.map ~f:trim @@ String.split_on_char ~sep:',' line)
                      ~init:syms
                      ~f:(get_syms ~mapper ~typ:(Option.value_exn typ) ~prefix:aprefix)
                    |> process_lines ?typ
          in
          process_lines syms
        )
      in

      let h_file_name = prefix ^ Hashtbl.find template "h_suffix" in

      Printf.fprintf c_file {|
#include <math.h>
#include <unicorn/unicorn.h>
#include "unicorn_poly_var_syms.h"
#include "%s"

|} h_file_name;

      let prefix_upper = String.uppercase_ascii prefix in

      Printf.fprintf h_file {|
#ifndef _ML_UNICORN_%s_STUBS_H_
#define _ML_UNICORN_%s_STUBS_H_

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/callback.h>

|} prefix_upper prefix_upper;

      let c2ml = Buffer.create 100 in
      let ml2c = Buffer.create 100 in
      let mlty = Buffer.create 100 in
      let mlcmp = Buffer.create 100 in

      Hashtbl.iter (fun k v ->
          let k_lower = String.lowercase_ascii k in
          Printf.fprintf h_file {|extern value ml_unicorn_to_%s(int v);
extern int ml_%s_to_unicorn(value v);
extern value ml_%s_to_unicorn_int(value v);
extern value ml_int_unicorn_to_%s(value v);
|} k_lower k_lower k_lower k_lower;

          Printf.bprintf c2ml {|value ml_unicorn_to_%s(int v) {
  CAMLparam0();
  switch (v) {
|} k_lower;

          Printf.bprintf ml2c {|int ml_%s_to_unicorn(value v) {
  CAMLparam1(v);
  switch (v) {
|} k_lower;

          let ml_mod = String.split_on_char ~sep:'_' k_lower |>
                       (if prefix = "uc" then List.tl else List.drop 2) |>
                       List.map ~f:String.capitalize_ascii |>
                       String.concat ~sep:""
          in

          Printf.bprintf mlty {|module %s = struct
  type t  = private int
  type id = [
|} ml_mod;

          Printf.bprintf mlcmp {|int ml_%s_compare(value u, value v) {
  CAMLparam2(u, v);
  int x = ml_%s_to_unicorn(u) - ml_%s_to_unicorn(v);
  CAMLreturn(Val_int(x==0 ? 0 : x/abs(x)));
}
|} k_lower k_lower k_lower;

          Syms.Map.iter (fun (sym_name, sym) vv ->
              let open Option in
              if String.starts_with "uc_" prefix then (
                if (Hashtbl.find_opt variant_mapping k_lower >>| fun tm -> Hashtbl.mem tm sym) <> Some true then (
                  Printf.bprintf c2ml {|  case %s:
    CAMLreturn(%s);
|} vv sym_name
                );
                Printf.bprintf ml2c {|  case %s:
    CAMLreturn(%s);
|} sym_name vv;
                Printf.bprintf mlty {|    | `%s
|} sym
              ) else if (Hashtbl.find_opt variant_mapping k_lower >>| fun tm -> Hashtbl.mem tm sym) <> Some true then (
                Printf.bprintf c2ml {|  case %s:
    CAMLreturn(%s);
|} vv sym_name;
                Printf.bprintf ml2c {|  case %s:
    CAMLreturn(%s);
|} sym_name vv;
                Printf.bprintf mlty {|    | `%s
|} sym
              )
            ) v;

          Printf.bprintf c2ml {|  default:
    caml_invalid_argument("ml_unicorn_%s: impossible value");
  }
}

value ml_%s_to_unicorn_int(value v) {
  CAMLparam1(v);
  CAMLreturn(Val_int(ml_%s_to_unicorn(v)));
}

|} k_lower k_lower k_lower;

          Printf.bprintf ml2c {|  default:
    caml_invalid_argument("ml_unicorn_%s: impossible value");
  }
}

value ml_int_unicorn_to_%s(value v) {
  CAMLparam1(v);
  CAMLreturn(ml_unicorn_to_%s(Int_val(v)));
}

|} k_lower k_lower k_lower;

          Printf.bprintf mlty {|  ]

  external of_id : id -> t = "ml_%s_to_unicorn_int"
  external to_id : t -> id = "ml_int_unicorn_to_%s"

|} k_lower k_lower;

          Syms.Map.iter (fun (_, sym) _ ->
              let open Option in
              let sym' = if String.starts_with "uc_" prefix then
                  sym
                else match Hashtbl.find_opt variant_mapping k_lower >>= fun tm -> Hashtbl.find_opt tm sym with
                  | Some sym' -> sym'
                  | _ -> sym
              in
              let name = match String.lowercase_ascii sym with
                | "and" | "asr" | "class" | "for" | "in" | "lsl"
                | "lsr" | "mod" | "not" | "or" | "exception" as v -> v ^ "_"
                | v -> v
              in
              Printf.bprintf mlty {|  let %s = of_id `%s
|} name sym'
            ) v;

          Printf.bprintf mlty "end\n\n";
        ) mapper;

      Printf.fprintf h_file {|#endif|};

      Buffer.output_buffer c_file c2ml;
      Buffer.output_buffer c_file ml2c;
      Buffer.output_buffer c_file mlcmp;

      Buffer.output_buffer ml_file mlty;

      close_out ml_file;
      close_out c_file;
      close_out h_file;
      syms'
    )
  in
  let ph_file = open_out "unicorn_poly_var_syms.h" in

  Printf.fprintf ph_file {|#ifndef _UNICORN_POLY_VAR_SYMS_H_
#define _UNICORN_POLY_VAR_SYMS_H_
|};

  Syms.Set.iter (fun (name, sym) ->
      Printf.fprintf ph_file {|#define %s (%d)
|} name (caml_hash_variant sym)
    ) syms;

  Printf.fprintf ph_file "\n#endif\n";
  close_out ph_file

let () =
  try generate () with _ -> print_endline "error generating bindings"
