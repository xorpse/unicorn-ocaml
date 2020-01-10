/*
 * Unicorn engine bindings for OCaml
 *
 * By Sam L. Thomas (xorpse) <st@xv.ax>, 2019 --
 *
 */

#include <string.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/version.h>

// from stdint
#include <uint8.h>
#include <uint16.h>
#include <uint32.h>
#include <uint64.h>

#include <unicorn/arm.h>
#include <unicorn/arm64.h>
#include <unicorn/unicorn.h>
#include <unicorn/m68k.h>
#include <unicorn/mips.h>
#include <unicorn/sparc.h>
#include <unicorn/x86.h>

#include "arm64_const_stubs.h"
#include "arm_const_stubs.h"
#include "m68k_const_stubs.h"
#include "mips_const_stubs.h"
#include "sparc_const_stubs.h"
#include "x86_const_stubs.h"

#include "unicorn_poly_var_syms.h"
#include "uc_const_stubs.h"

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)
#define Val_emptyarray Atom(0)

static CAMLprim value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

static CAMLprim value caml_copy_uint8_array(uint8_t *array, size_t len) {
  CAMLparam0();
  CAMLlocal1(a);

  a = caml_alloc(len, 0);

  for (size_t i = 0; i < len; i++) {
    Store_field(a, i, Val_int(array[i]));
  }

  CAMLreturn(a);
}

#define ARR_SIZE(a) (sizeof(a) / sizeof(*a))
#define Unicorn_handle_val(v) (*(uc_engine **)Data_custom_val(v))

static void ml_unicorn_finalise_handle(value h) {
  CAMLparam1(h);
  puts("HERE");
  uc_close(Unicorn_handle_val(h));
  CAMLreturn0;
}

static struct custom_operations ml_unicorn_handle_custom_ops = {
    (char *)"ml_unicorn_handle_custom_ops",
    ml_unicorn_finalise_handle,
    custom_compare_default,
    custom_compare_ext_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
#if OCAML_VERSION_MAJOR >= 4 && OCAML_VERSION_MINOR >= 8
    NULL, // custom_fixed_length
#endif
};

static value ml_unicorn_alloc_handle(uc_engine *handle) {
  CAMLparam0();
  CAMLlocal1(h);
  h = caml_alloc_custom(&ml_unicorn_handle_custom_ops, sizeof(uc_engine *), 0, 1);
  memcpy(Data_custom_val(h), &handle, sizeof(uc_engine *));
  CAMLreturn(h);
}

CAMLprim value ml_unicorn_create(value arch, value mode) {
  CAMLparam2(arch, mode);

  uc_err err;
  uc_engine *handle;

  if ((err = uc_open(Int_val(arch), Int_val(mode), &handle)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(ml_unicorn_alloc_handle(handle));
}

typedef struct HookCB {
  value f;
  value v;
} HookCB;

CAMLprim void ml_unicorn_hook_hndl_insn_out(uc_engine *_uc, uint32_t port, int size, uint32_t val, void *user_data) {
  CAMLparam0();

  HookCB *ud = (HookCB *)user_data;
  value args[] = {caml_copy_int32(port), Val_int(size), caml_copy_int32(val), ud->v};

  ud->v = caml_callbackN(ud->f, ARR_SIZE(args), args);
  CAMLreturn0;
}

CAMLprim uint32_t ml_unicorn_hook_hndl_insn_in(uc_engine *uc, uint32_t port, int size, void *user_data) {
  CAMLparam0();
  CAMLlocal1(nv);

  HookCB *ud = (HookCB *)user_data;
  value args[] = {ml_unicorn_alloc_handle(uc), caml_copy_int32(port), Val_int(size), ud->v};

  // int32 * 'a
  nv = caml_callbackN(ud->f, ARR_SIZE(args), args);

  ud->v = Field(nv, 1);

  CAMLreturnT(uint32_t, Int32_val(Field(nv, 0)));
}

CAMLprim bool ml_unicorn_hook_hndl_memev(uc_engine *_uc, uc_mem_type type, uint64_t address, int size, int64_t val, void *user_data) {
  CAMLparam0();
  CAMLlocal1(nv);

  bool cont = false;

  HookCB *ud = (HookCB *)user_data;
  value args[] = {Val_int(type), caml_copy_int64(address), Val_int(size), caml_copy_int64(val), ud->v};

  nv = caml_callbackN(ud->f, ARR_SIZE(args), args);

  switch (Tag_val(nv)) {
  case 0: // Continue
    cont = true;
    break;
  case 1: // Stop
    cont = false;
    break;
  }

  ud->v = Field(nv, 0);

  CAMLreturnT(bool, cont);
}

CAMLprim static void ml_unicorn_hook_hndl_mem(uc_engine *_uc, uc_mem_type type, uint64_t address, int size, int64_t val, void *user_data) {
  CAMLparam0();
  CAMLlocal4(type_v, address_v, size_v, val_v);

  HookCB *ud = (HookCB *)user_data;

  type_v = Val_int(type);
  address_v = caml_copy_int64(address);
  size_v = Val_int(size);
  val_v = caml_copy_int64(val);

  value args[] = {type_v, address_v, size_v, val_v, ud->v};

  ud->v = caml_callbackN(ud->f, ARR_SIZE(args), args);

  CAMLreturn0;
}

CAMLprim static void ml_unicorn_hook_hndl_intr(uc_engine *_uc, uint32_t intno, void *user_data) {
  CAMLparam0();
  CAMLlocal1(intno_v);

  HookCB *ud = (HookCB *)user_data;

  intno_v = caml_copy_int32(intno);

  value args[] = {intno_v, ud->v};

  ud->v = caml_callbackN(ud->f, ARR_SIZE(args), args);
  CAMLreturn0;
}

static void ml_unicorn_hook_hndl_code(uc_engine *_uc, uint64_t address, uint32_t size, void *user_data) {
  CAMLparam0();
  CAMLlocal2(address_v, size_v);

  HookCB *ud = (HookCB *)user_data;

  address_v = caml_copy_int64(address);
  size_v = caml_copy_int32(size);

  value args[] = {address_v, size_v, ud->v};

  ud->v = caml_callbackN(ud->f, ARR_SIZE(args), args);

  CAMLreturn0;
}

CAMLprim value ml_unicorn_hook_add(value engine, value type, value hook, value init, value saddr, value eaddr) {
  // uc_hook_add gives handle; clean-up, we call uc_hook_del
  // therefore we need to store the callbacks within the handle
  // itself
  CAMLparam5(engine, type, hook, init, saddr);
  CAMLxparam1(eaddr);

  uc_err err = UC_ERR_OK;
  uc_hook hh = 0; // size_t
  uc_hook_type ht = Int_val(type);

  uint64_t sa = Uint64_val(saddr), ea = Uint64_val(eaddr);

  HookCB* ud = caml_stat_alloc(sizeof(HookCB));
  ud->f = hook;
  caml_register_global_root(&(ud->f));
  ud->v = init;
  caml_register_global_root(&(ud->v));

  if (ht & UC_HOOK_INTR) {
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, &ml_unicorn_hook_hndl_intr, (void *)ud, sa, ea);
  } else if (ht & (UC_HOOK_CODE | UC_HOOK_BLOCK)) {
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, &ml_unicorn_hook_hndl_code, (void *)ud, sa, ea);
  } else if (ht & UC_HOOK_MEM_VALID) {
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, ml_unicorn_hook_hndl_mem, (void *)ud, sa, ea);
  } else if (ht & UC_HOOK_MEM_INVALID) {
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, ml_unicorn_hook_hndl_memev, (void *)ud, sa, ea);
  } else {
    err = UC_ERR_HOOK;
  }

  if (err != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(caml_copy_int64(hh));
}

CAMLprim value ml_unicorn_hook_add_bytecode(value *argv, int _argc) {
  return ml_unicorn_hook_add(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value ml_unicorn_hook_add_insn(value engine, value type, value hook, value init, value insn) {
  CAMLparam5(engine, type, hook, init, insn);

  uc_err err = UC_ERR_OK;
  uc_hook hh;
  uc_hook_type ht = Int_val(type);

  HookCB* ud = caml_stat_alloc(sizeof(HookCB));
  ud->f = hook;
  caml_register_global_root(&(ud->f));
  ud->v = init;
  caml_register_global_root(&(ud->v));

  switch (Int_val(insn)) {
  case UC_X86_INS_IN:
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, ml_unicorn_hook_hndl_insn_in, (void *)ud, 0, 0, UC_X86_INS_IN);
    break;
  case UC_X86_INS_OUT:
    err = uc_hook_add(Unicorn_handle_val(engine), &hh, ht, ml_unicorn_hook_hndl_insn_out, (void *)ud, 0, 0, UC_X86_INS_OUT);
    break;
  default:
    err = UC_ERR_HOOK;
    break;
  }

  if (err != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(caml_copy_int64(hh));
}

CAMLprim value ml_unicorn_hook_del(value engine, value hook) {
  CAMLparam2(engine, hook);
  uc_err err = UC_ERR_OK;

  if ((err = uc_hook_del(Unicorn_handle_val(engine), Int64_val(hook))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_version(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(majmin);

  unsigned int major, minor; uc_version(&major, &minor);

  majmin = caml_alloc(2, 0);
  Store_field(majmin, 0, Val_int(major));
  Store_field(majmin, 1, Val_int(minor));

  CAMLreturn(majmin);
}

CAMLprim value ml_unicorn_query(value engine, value query) {
  CAMLparam2(engine, query);

  uc_err err;
  size_t result = 0;
  if ((err = uc_query(Unicorn_handle_val(engine), Int_val(query), &result)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(caml_copy_int64(result));
}

CAMLprim value ml_unicorn_read_uint8(value engine, value address) {
  CAMLparam2(engine, address);

  uc_err err;
  uint8_t v = 0;
  if ((err = uc_mem_read(Unicorn_handle_val(engine), (uint64_t)Uint8_val(address), (void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_uint8(v));
}

CAMLprim value ml_unicorn_read_uint16(value engine, value address) {
  CAMLparam2(engine, address);

  uc_err err;
  uint16_t v = 0;
  if ((err = uc_mem_read(Unicorn_handle_val(engine), (uint64_t)Uint16_val(address), (void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_uint16(v));
}

CAMLprim value ml_unicorn_read_uint32(value engine, value address) {
  CAMLparam2(engine, address);

  uc_err err;
  uint32_t v = 0;
  if ((err = uc_mem_read(Unicorn_handle_val(engine), (uint64_t)Uint32_val(address), (void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(copy_uint32(v));
}

CAMLprim value ml_unicorn_read_uint64(value engine, value address) {
  CAMLparam2(engine, address);

  uc_err err;
  uint64_t v = 0;
  if ((err = uc_mem_read(Unicorn_handle_val(engine), (uint64_t)Uint64_val(address), (void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(copy_uint64(v));
}

CAMLprim value ml_unicorn_write_uint8(value engine, value address, value val) {
  CAMLparam3(engine, address, val);

  uc_err err;
  uint8_t v = Uint8_val(val);
  if ((err = uc_mem_write(Unicorn_handle_val(engine), (uint64_t)Uint64_val(address), (const void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_write_uint16(value engine, value address, value val) {
  CAMLparam3(engine, address, val);

  uc_err err;
  uint16_t v = Uint16_val(val);
  if ((err = uc_mem_write(Unicorn_handle_val(engine), (uint64_t)Uint64_val(address), (const void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_write_uint32(value engine, value address, value val) {
  CAMLparam3(engine, address, val);

  uc_err err;
  uint32_t v = Uint32_val(val);
  if ((err = uc_mem_write(Unicorn_handle_val(engine), (uint64_t)Uint64_val(address), (const void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_write_uint64(value engine, value address, value val) {
  CAMLparam3(engine, address, val);

  uc_err err;
  uint64_t v = Uint64_val(val);
  if ((err = uc_mem_write(Unicorn_handle_val(engine), (uint64_t)Uint64_val(address), (const void *)&v, sizeof(v))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_read_bytes(value engine, value address, value size) {
  CAMLparam3(engine, address, size);
  CAMLlocal1(bytes);

  if (Int_val(size) < 0) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(UC_ERR_ARG));
  }

  uc_err err;
  bytes = caml_alloc_string(size);

  if ((err = uc_mem_read(Unicorn_handle_val(engine), Uint64_val(address), (void *)Bytes_val(bytes), (size_t)Int_val(size))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(bytes);
}

CAMLprim value ml_unicorn_write_bytes(value engine, value address, value bytes) {
  CAMLparam3(engine, address, bytes);

  uc_err err;
  if ((err = uc_mem_write(Unicorn_handle_val(engine), Uint64_val(address), (const void *)Bytes_val(bytes), (size_t)caml_string_length(bytes))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_map(value engine, value address, value size, value perms) {
  CAMLparam4(engine, address, size, perms);

  if (Int_val(size) < 0) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(UC_ERR_ARG));
  }

  uc_err err;
  if ((err = uc_mem_map(Unicorn_handle_val(engine), Uint64_val(address), (size_t)Int_val(size), Int_val(perms))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_unmap(value engine, value address, value size) {
  CAMLparam3(engine, address, size);

  if (Int_val(size) < 0) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(UC_ERR_ARG));
  }

  uc_err err;
  if ((err = uc_mem_unmap(Unicorn_handle_val(engine), Uint64_val(address), (size_t)Int_val(size))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_protect(value engine, value address, value size, value perms) {
  CAMLparam4(engine, address, size, perms);

  if (Int_val(size) < 0) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(UC_ERR_ARG));
  }

  uc_err err;
  if ((err = uc_mem_protect(Unicorn_handle_val(engine), Uint64_val(address), (size_t)Int_val(size), Int_val(perms))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_reg_write_uint8(value engine, value id, value val) {
  CAMLparam3(engine, id, val);

  uc_err err;
  uint8_t v = Uint8_val(val);
  if ((err = uc_reg_write(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_reg_write_uint16(value engine, value id, value val) {
  CAMLparam3(engine, id, val);

  uc_err err;
  uint64_t v = Uint16_val(val);
  if ((err = uc_reg_write(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}
CAMLprim value ml_unicorn_reg_write_uint32(value engine, value id, value val) {
  CAMLparam3(engine, id, val);

  uc_err err;
  uint32_t v = Uint32_val(val);
  if ((err = uc_reg_write(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}
CAMLprim value ml_unicorn_reg_write_uint64(value engine, value id, value val) {
  CAMLparam3(engine, id, val);

  uc_err err;
  uint64_t v = Uint64_val(val);
  if ((err = uc_reg_write(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_reg_read_uint8(value engine, value id) {
  CAMLparam2(engine, id);

  uc_err err;
  uint8_t v;
  if ((err = uc_reg_read(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_uint8(v));
}

CAMLprim value ml_unicorn_reg_read_uint16(value engine, value id) {
  CAMLparam2(engine, id);

  uc_err err;
  uint16_t v;
  if ((err = uc_reg_read(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_uint16(v));
}

CAMLprim value ml_unicorn_reg_read_uint32(value engine, value id) {
  CAMLparam2(engine, id);

  uc_err err;
  uint32_t v;
  if ((err = uc_reg_read(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(copy_uint32(v));
}

CAMLprim value ml_unicorn_reg_read_uint64(value engine, value id) {
  CAMLparam2(engine, id);

  uc_err err;
  uint64_t v;
  if ((err = uc_reg_read(Unicorn_handle_val(engine), Int_val(id), &v)) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(copy_uint64(v));
}

CAMLprim value ml_unicorn_start(value engine, value address, value until, value timeout, value count)
{
  CAMLparam5(engine, address, until, timeout, count);

  if (Int_val(count) < 0) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(UC_ERR_ARG));
  }


  uc_err err;
  if ((err = uc_emu_start(Unicorn_handle_val(engine), Uint64_val(address), Uint64_val(until), Uint64_val(timeout), Int_val(count))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}

CAMLprim value ml_unicorn_stop(value engine)
{
  CAMLparam1(engine);

  uc_err err;
  if ((err = uc_emu_stop(Unicorn_handle_val(engine))) != UC_ERR_OK) {
    caml_raise_with_arg(*caml_named_value("Unicorn_error"),
                        Val_int(err));
  }

  CAMLreturn(Val_unit);
}
