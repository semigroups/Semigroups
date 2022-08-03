#############################################################################
##
##  options.g
##  Copyright (C) 2013-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SEMIGROUPS.DefaultOptionsRec :=
  rec(small      := false,
      hashlen    := 12517,
      regular    := false,
      acting     := true,
      batch_size := 8192,
      nr_threads := 4,
      cong_by_ker_trace_threshold := 10 ^ 5);

SEMIGROUPS.ProcessOptionsRec := function(defaults, opts)
  local name;
  for name in RecNames(defaults) do
    if not IsBound(opts.(name)) then
      opts.(name) := defaults.(name);
    elif TNUM_OBJ(opts.(name)) <> TNUM_OBJ(defaults.(name)) then
      Info(InfoWarning, 1, "Expected a ", TNAM_OBJ(defaults.(name)),
           " for option \"", name, "\", but got a ", TNAM_OBJ(opts.(name)));
      Info(InfoWarning, 1, "Ignoring the value of option \"", name, "\"");
      opts.(name) := defaults.(name);
    fi;
  od;
  for name in RecNames(opts) do
    if not IsBound(defaults.(name)) then
      Info(InfoWarning, 1, "Ignoring unknown option \"", name, "\"");
      Unbind(opts.(name));
    fi;
  od;
  return opts;
end;

SEMIGROUPS.OptionsRec := function(S)
  if not IsBound(S!.opts) then
    return SEMIGROUPS.DefaultOptionsRec;
  fi;
  return S!.opts;
end;

DeclareUserPreference(rec(
  name := "ViewObj",
  description := [Concatenation("options for the viewing various objects in ",
                  "the Semigroups package")],
  default := "gap-library",
  values := ["semigroups-pkg", "gap-library"],
  multi := false,
  package := "semigroups"));
