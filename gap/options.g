#############################################################################
##
#W  options.g
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

BindGlobal("SEMIGROUPS_DefaultOptionsRec",
  rec(small   := false,
      hashlen := rec(S := 251, M := 6257, L := 25013),
      regular := false,
      acting  := true,
      report  := false));

MakeReadWriteGlobal("SEMIGROUPS_DefaultOptionsRec");

BindGlobal("SEMIGROUPS_ProcessOptionsRec",
function(opts)
  local n, x;

  for x in RecNames(SEMIGROUPS_DefaultOptionsRec) do
    if not IsBound(opts.(x)) then
      opts.(x) := SEMIGROUPS_DefaultOptionsRec.(x);
    fi;
  od;

  if IsBound(opts.hashlen) and IsPosInt(opts.hashlen) then
    n := opts.hashlen;
    opts.hashlen := rec(S := NextPrimeInt(Int(n / 100)),
                        M := NextPrimeInt(Int(n / 4)),
                        L := NextPrimeInt(n));
  fi;

  return opts;
end);

BindGlobal("SEMIGROUPS_OptionsRec",
function(S)
  if not IsBound(S!.opts) then
    return SEMIGROUPS_DefaultOptionsRec;
  fi;
  return S!.opts;
end);
