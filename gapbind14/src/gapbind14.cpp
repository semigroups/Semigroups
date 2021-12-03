//
// gapbind14
// Copyright (C) 2020 James D. Mitchell
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "gapbind14/gapbind14.hpp"

#define GVAR_ENTRY(srcfile, name, nparam, params)                              \
  { #name, nparam, params, (GVarFunc)name, srcfile ":Func" #name }

namespace gapbind14 {
UInt T_GAPBIND14_OBJ = 0;

Module &get_module() {
  static Module MODULE;
  return MODULE;
}

namespace {

Obj TheTypeTGapBind14Obj;

////////////////////////////////////////////////////////////////////////
// Required kernel functions
////////////////////////////////////////////////////////////////////////

Obj TGapBind14ObjTypeFunc(Obj o) { return TheTypeTGapBind14Obj; }

void TGapBind14ObjPrintFunc(Obj o) { get_module().print(o); }

void TGapBind14ObjSaveFunc(Obj o) { get_module().save(o); }

void TGapBind14ObjLoadFunc(Obj o) { get_module().load(o); }

Obj TGapBind14ObjCopyFunc(Obj o, Int mut) { return o; }

void TGapBind14ObjCleanFunc(Obj o) {}

void TGapBind14ObjFreeFunc(Obj o) { get_module().free(o); }

////////////////////////////////////////////////////////////////////////
// Copied from gap/src/modules.c, should be exposed in header TODO(later)
////////////////////////////////////////////////////////////////////////

static Obj ValidatedArgList(const char *name, int nargs, const char *argStr) {
  Obj args = ArgStringToList(argStr);
  int len = LEN_PLIST(args);
  if (nargs >= 0 && len != nargs)
    fprintf(stderr,
            "#W %s takes %d arguments, but argument string is '%s'"
            " which implies %d arguments\n",
            name, nargs, argStr, len);
  return args;
}

static void SetupFuncInfo(Obj func, const Char *cookie) {
  // The string <cookie> usually has the form "PATH/TO/FILE.c:FUNCNAME".
  // We check if that is the case, and if so, split it into the parts before
  // and after the colon. In addition, the file path is cut to only contain
  // the last two '/'-separated components.
  const Char *pos = strchr(cookie, ':');
  if (pos) {
    Obj location = MakeImmString(pos + 1);

    Obj filename;
    char buffer[512];
    Int len = 511 < (pos - cookie) ? 511 : pos - cookie;
    memcpy(buffer, cookie, len);
    buffer[len] = 0;

    Char *start = strrchr(buffer, '/');
    if (start) {
      while (start > buffer && *(start - 1) != '/')
        start--;
    } else {
      start = buffer;
    }
    filename = MakeImmString(start);

    Obj body_bag = NewBag(T_BODY, sizeof(BodyHeader));
    SET_FILENAME_BODY(body_bag, filename);
    SET_LOCATION_BODY(body_bag, location);
    SET_BODY_FUNC(func, body_bag);
    CHANGED_BAG(body_bag);
    CHANGED_BAG(func);
  }
}

Obj IsValidGapbind14Object(Obj self, Obj arg1) {
  if (TNUM_OBJ(arg1) != T_GAPBIND14_OBJ) {
    ErrorQuit("expected gapbind14 object but got %s!", (Int)TNAM_OBJ(arg1), 0L);
  }
  GAPBIND14_ASSERT(SIZE_OBJ(arg1) == 2);
  return (ADDR_OBJ(arg1)[1] != nullptr ? True : False);
}

StructGVarFunc GVarFuncs[] = {
    GVAR_ENTRY("gapbind14.cpp", IsValidGapbind14Object, 1, "arg1"),
    {0, 0, 0, 0, 0}};
} // namespace

void check_args(Obj args, size_t n) {
  if (!IS_LIST(args)) {
    ErrorQuit("expected the argument to be a list, found %s",
              (Int)TNAM_OBJ(args), 0L);
  } else if (LEN_LIST(args) != n) {
    ErrorQuit("expected the argument to be a list of length %d, found %d",
              (Int)n, (Int)LEN_LIST(args));
  }
}
// Subtype implementations

Subtype::Subtype(std::string nm, gapbind14_sub_type sbtyp)
    : _name(nm), _subtype(sbtyp) {
  static std::unordered_set<gapbind14_sub_type> defined;
  if (defined.find(sbtyp) != defined.end()) {
    throw std::runtime_error("Subtype " + to_string(sbtyp) +
                             " already registered!");
  } else {
    defined.insert(sbtyp);
  }
}

void init_kernel(Module &m) {
  InitHdlrFuncsFromTable(GVarFuncs);

  GAPBIND14_MODULE_IMPL(m);
  auto &mm = get_module();
  mm = m;
  InitHdlrFuncsFromTable(mm.funcs());

  for (auto ptr : mm) {
    InitHdlrFuncsFromTable(mm.mem_funcs(ptr->name()));
  }

  UInt &PKG_TNUM = T_GAPBIND14_OBJ;
  PKG_TNUM = RegisterPackageTNUM("TGapBind14", TGapBind14ObjTypeFunc);

  PrintObjFuncs[PKG_TNUM] = TGapBind14ObjPrintFunc;
  SaveObjFuncs[PKG_TNUM] = TGapBind14ObjSaveFunc;
  LoadObjFuncs[PKG_TNUM] = TGapBind14ObjLoadFunc;

  CopyObjFuncs[PKG_TNUM] = &TGapBind14ObjCopyFunc;
  CleanObjFuncs[PKG_TNUM] = &TGapBind14ObjCleanFunc;
  IsMutableObjFuncs[PKG_TNUM] = &AlwaysNo;

  InitMarkFuncBags(PKG_TNUM, MarkNoSubBags);
  InitFreeFuncBag(PKG_TNUM, TGapBind14ObjFreeFunc);

  InitCopyGVar("TheTypeTGapBind14Obj", &TheTypeTGapBind14Obj);
}

void init_library(Module &m) {
  InitGVarFuncsFromTable(GVarFuncs);
  StructGVarFunc const *tab = m.funcs();

  // init functions from m in the record named m.module_name()
  // This is done to avoid polluting the global namespace
  Obj global_rec = NEW_PREC(0);
  SET_LEN_PREC(global_rec, 0);

  for (Int i = 0; tab[i].name != 0; i++) {
    UInt gvar = GVarName(tab[i].name);
    Obj name = NameGVar(gvar);
    Obj args = ValidatedArgList(tab[i].name, tab[i].nargs, tab[i].args);
    Obj func = NewFunction(name, tab[i].nargs, args, tab[i].handler);
    SetupFuncInfo(func, tab[i].cookie);
    AssPRec(global_rec, RNamName(tab[i].name), func);
  }
  for (auto ptr : m) {
    tab = m.mem_funcs(ptr->name());
    Obj class_rec = NEW_PREC(0);
    SET_LEN_PREC(class_rec, 0);

    for (Int i = 0; tab[i].name != 0; i++) {
      UInt gvar = GVarName(tab[i].name);
      Obj name = NameGVar(gvar);
      Obj args = ValidatedArgList(tab[i].name, tab[i].nargs, tab[i].args);
      Obj func = NewFunction(name, tab[i].nargs, args, tab[i].handler);
      SetupFuncInfo(func, tab[i].cookie);
      AssPRec(class_rec, RNamName(tab[i].name), func);
    }
    AssPRec(global_rec, RNamName(ptr->name().c_str()), class_rec);
  }

  MakeImmutable(global_rec);
  AssReadOnlyGVar(GVarName(m.module_name()), global_rec);
}
} // namespace gapbind14
