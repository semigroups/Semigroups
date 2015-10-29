#############################################################################
##
#W  constructions.gi
#Y  Copyright (C) 2015                                    James D. Mitchell
##                                                        Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

SEMIGROUPS.AsXSemigroup := function(filt)
  if filt = IsTransformationSemigroup then
    return AsTransformationSemigroup;
  elif filt = IsPartialPermSemigroup then
    return AsPartialPermSemigroup;
  elif filt = IsBipartitionSemigroup then
    return AsBipartitionSemigroup;
  elif filt = IsBlockBijectionSemigroup then
    return AsBlockBijectionSemigroup;
  elif filt = IsPBRSemigroup then
    return AsPBRSemigroup;
  elif filt = IsBooleanMatSemigroup then
    return AsBooleanMatSemigroup;
  fi;
  return fail;
end;

# Also have:
#  IsMaxPlusMatrixSemigroup
#  IsMinPlusMatrixSemigroup
#  IsTropicalMaxPlusMatrixSemigroup
#  IsTropicalMinPlusMatrixSemigroup
#  IsProjectiveMaxPlusMatrixSemigroup
#  IsNTPMatrixSemigroup
#  IsMatrixOverPrimeFieldSemigroup
#  IsIntegerMatrixSemigroup

SEMIGROUPS.StandardExampleApplyAttributes := function(arg)
  local func, S, param, m, r, n;

  func := arg[1];
  S := arg[2];
  param := arg{[3 .. Length(arg)]};

  # Trivial semigroup
  if func = TrivialSemigroup then
    SetIsTrivial(S, true);

  # Zero semigroup
  elif func = ZeroSemigroup then
    n := param[1];
    SetSize(S, n);
    SetIsZeroSemigroup(S, true);
    SetMultiplicativeZero(S, GeneratorsOfSemigroup(S)[1] ^ 2);
    if IsTrivial(S) then
      SetAsList(S, GeneratorsOfSemigroup(S));
    else
      SetIsGroupAsSemigroup(S, false);
      SetIsRegularSemigroup(S, false);
      if n > 2 then
        SetIsMonogenicSemigroup(S, false);
      fi;
      SetAsList(S, Concatenation(GeneratorsOfSemigroup(S),
                                 [MultiplicativeZero(S)]));
    fi;

  # Monogenic semigroup
  elif func = MonogenicSemigroup then
    m := param[1];
    r := param[2];
    SetSize(S, m + r - 1);
    SetIsMonogenicSemigroup(S, true);
    if m = 1 then
      if not IsGroup(S) then
        SetIsGroupAsSemigroup(S, true);
      fi;
    else
      SetIsGroupAsSemigroup(S, false);
      SetIsRegularSemigroup(S, false);
    fi;

    if r = 1 and m < 3 then
      SetIsZeroSemigroup(S, true);
    else
      SetIsZeroSemigroup(S, false);
    fi;

  # Rectangular band
  elif func = RectangularBand then
    m := param[1];
    n := param[2];
    SetSize(S, m * n);
    SetIsRectangularBand(S, true);
    SetNrRClasses(S, m);
    SetNrLClasses(S, n);
    if not (m = 1 and n = 1) then
      SetIsGroupAsSemigroup(S, false);
      SetIsZeroSemigroup(S, false);
      SetIsTrivial(S, false);
      if m = 1 then
        SetIsRightZeroSemigroup(S, true);
      else
        SetIsRightZeroSemigroup(S, false);
      fi;
      if n = 1 then
        SetIsLeftZeroSemigroup(S, true);
      else
        SetIsLeftZeroSemigroup(S, false);
      fi;
    else
      SetIsTrivial(S, true);
    fi;
  fi;
end;

# For 1-parameter constructions
SEMIGROUPS.InstallConstructors1 := function(func, exclude, param)
  local type, cons;
  cons := EvalString(Concatenation(NameFunction(func), "Cons"));
  for type in SEMIGROUPS_Types do
    if not type in exclude then
      InstallMethod(cons, Concatenation([type], param),
      function(filt, n)
        local S;
        if SEMIGROUPS.AsXSemigroup(filt) <> fail then
          S := SEMIGROUPS.AsXSemigroup(filt)(func(IsBipartitionSemigroup, n));
          SEMIGROUPS.StandardExampleApplyAttributes(func, S, n);
          return S;
        fi;
        return fail;
      end);
    fi;
  od;
end;

# For 2-parameter constructions
SEMIGROUPS.InstallConstructors2 := function(func, exclude, param)
  local type, cons;
  cons := EvalString(Concatenation(NameFunction(func), "Cons"));
  for type in SEMIGROUPS_Types do
    if not type in exclude then
      InstallMethod(cons, Concatenation([type], param),
      function(filt, m, n)
        local S;
        if SEMIGROUPS.AsXSemigroup(filt) <> fail then
          S := SEMIGROUPS.AsXSemigroup(filt)(func(IsBipartitionSemigroup, m,
                                                  n));
          SEMIGROUPS.StandardExampleApplyAttributes(func, S, m, n);
          return S;
        fi;
        return fail;
      end);
    fi;
  od;
end;

# Trivial semigroup: main method

InstallGlobalFunction(TrivialSemigroup,
function(arg)
  local out;

  if Length(arg) = 0 then
    out := TrivialSemigroupCons(IsTransformationSemigroup, 0);
  elif Length(arg) = 1 and IsInt(arg[1]) and arg[1] >= 0 then
    out := TrivialSemigroupCons(IsTransformationSemigroup, arg[1]);
  elif Length(arg) = 1 and IsOperation(arg[1]) then
    out := TrivialSemigroupCons(arg[1], 0);
  elif Length(arg) = 2 and IsOperation(arg[1]) and IsInt(arg[2])
      and arg[2] >= 0 then
    out := TrivialSemigroupCons(arg[1], arg[2]);
  else
    ErrorMayQuit("Semigroups: TrivialSemigroup: usage,\n",
                 "the arguments must be a non-negative integer or ",
                 "a filter and a non-negative\ninteger,");
  fi;
  if out = fail then
    ErrorMayQuit("Semigroups: TrivialSemigroup: usage,\n",
                 "the requested filter is not supported,");
  fi;
  SEMIGROUPS.StandardExampleApplyAttributes(TrivialSemigroup, out, 0);
  return out;
end);

# Trivial semigroup: constructors

InstallMethod(TrivialSemigroupCons,
[IsTransformationSemigroup, IsInt],
function(filt, deg)
  if deg = 0 then
    return Semigroup(IdentityTransformation);
  fi;
  return Semigroup(ConstantTransformation(deg, 1));
end);

#

InstallMethod(TrivialSemigroupCons,
[IsPartialPermSemigroup, IsInt],
function(filt, n)
  return Semigroup(PartialPerm([1 .. n]));
end);

#

InstallMethod(TrivialSemigroupCons,
[IsBipartitionSemigroup, IsInt],
function(filt, deg)
  local n;
  n := Maximum(deg, 1);
  return Semigroup(Bipartition([Concatenation(List([1 .. n], x -> [-x, x]))]));
end);

#

InstallMethod(TrivialSemigroupCons,
[IsBlockBijectionSemigroup, IsInt],
function(filt, deg)
  return TrivialSemigroupCons(IsBipartitionSemigroup, deg);
end);

#

InstallMethod(TrivialSemigroupCons,
[IsPBRSemigroup, IsInt],
function(filt, deg)
  local n;
  n := Maximum(deg, 1);
  return Semigroup(IdentityPBR(n));
end);

#

InstallMethod(TrivialSemigroupCons,
[IsBooleanMatSemigroup, IsInt],
function(filt, deg)
  local n;
  n := Maximum(deg, 1);
  return Semigroup(BooleanMat(List([1 .. n], x -> BlistList([1 .. n], [x]))));
end);

# Trivial semigroup: other constructors

SEMIGROUPS.InstallConstructors1(TrivialSemigroup,
                                [IsTransformationSemigroup,
                                 IsPartialPermSemigroup,
                                 IsBipartitionSemigroup,
                                 IsBlockBijectionSemigroup,
                                 IsPBRSemigroup,
                                 IsBooleanMatSemigroup],
                                [IsInt]);

# Monogenic semigroup: main method

InstallGlobalFunction(MonogenicSemigroup,
function(arg)
  local filter, m, r, out;

  if Length(arg) = 2  then
    filter := IsTransformationSemigroup;
    m := arg[1];
    r := arg[2];
  elif Length(arg) = 3 then
    filter := arg[1];
    m := arg[2];
    r := arg[3];
  fi;

  if not IsBound(m) or not IsPosInt(m) or not IsPosInt(r)
      or not IsOperation(filter) then
    ErrorMayQuit("Semigroups: MonogenicSemigroup: usage,\n",
                 "the arguments must be two positive integers or a filter ",
                 "and a two positive\nintegers,");
  fi;

  out := MonogenicSemigroupCons(filter, m, r);
  if out = fail then
    ErrorMayQuit("Semigroups: MonogenicSemigroup: usage,\n",
                 "the requested filter is not supported,");
  fi;
  SEMIGROUPS.StandardExampleApplyAttributes(MonogenicSemigroup, out, m, r);
  return out;
end);

# Monogenic semigroup: constructors

InstallMethod(MonogenicSemigroupCons,
"for a filter and a positive integer and positive integer",
[IsTransformationSemigroup, IsPosInt, IsPosInt],
function(filter, m, r)
  local t;

  t := [1 .. r] + 1;
  t[r] := 1;

  if not m = 1 then # m = 1 specifies a cyclic group
    Append(t, [1 .. m] + r - 1);
  fi;

  return Semigroup(Transformation(t));
end);

#

InstallMethod(MonogenicSemigroupCons,
"for a filter and two positive integers",
[IsPartialPermSemigroup, IsPosInt, IsPosInt],
function(filter, m, r)
  local cyclic_group, nilpotent_offset, nilpotent, im;

  if m = 1 and r = 1 then
    return Semigroup(PartialPerm([], []));
  fi;

  if r = 1 then
    cyclic_group := [];
    nilpotent_offset := 0;
  else
    cyclic_group := [1 .. r] + 1;
    cyclic_group[r] := 1;
    nilpotent_offset := r;
  fi;
  nilpotent := [1 .. m - 1] + nilpotent_offset;
  im := Concatenation(cyclic_group, [0], nilpotent);

  return Semigroup(PartialPerm(im));
end);

#

InstallMethod(MonogenicSemigroupCons,
"for a filter and two positive integers",
[IsBipartitionSemigroup, IsPosInt, IsPosInt],
function(filter, m, r)
  return MonogenicSemigroupCons(IsBlockBijectionSemigroup, m, r);
end);

#

InstallMethod(MonogenicSemigroupCons,
"for a filter and two positive integers",
[IsBlockBijectionSemigroup, IsPosInt, IsPosInt],
function(filter, m, r)
  local out, offset, i;

  if m = 1 and r = 1 then
    return Semigroup(Bipartition([[1, -1]]));
  fi;

  out := [];
  if r = 1 then
    offset := 1;
  else
    for i in [1 .. r - 1] do
      Add(out, [i, -i - 1]);
    od;
    Add(out, [r, -1]);
    offset := r + 1;
  fi;

  if not m = 1 then
    Add(out, [offset, -offset, offset + 1, -(offset + m)]);
    for i in [offset + 2 .. offset + m] do
      Add(out, [i, -i + 1]);
    od;
  fi;

  return Semigroup(Bipartition(out));
end);

# Monogenic semigroup: other constructors

SEMIGROUPS.InstallConstructors2(MonogenicSemigroup,
                                [IsTransformationSemigroup,
                                 IsPartialPermSemigroup,
                                 IsBipartitionSemigroup,
                                 IsBlockBijectionSemigroup,
                                 IsReesMatrixSemigroup,
                                 IsReesZeroMatrixSemigroup],
                                [IsPosInt, IsPosInt]);

# Rectangular band: main method

InstallGlobalFunction(RectangularBand,
function(arg)
  local filter, m, n, out;

  if Length(arg) = 2  then
    filter := IsTransformationSemigroup;
    m := arg[1];
    n := arg[2];
  elif Length(arg) = 3 then
    filter := arg[1];
    m := arg[2];
    n := arg[3];
  fi;

  if not IsBound(m) or not IsPosInt(m) or not IsPosInt(n)
      or not IsOperation(filter) then
    ErrorMayQuit("Semigroups: RectangularBand: usage,\n",
                 "the arguments must be two positive integers or a filter ",
                 "and a two positive\nintegers,");
  fi;

  out := RectangularBandCons(filter, m, n);
  if out = fail then
    ErrorMayQuit("Semigroups: RectangularBand: usage,\n",
                 "the requested filter is not supported,");
  fi;
  SEMIGROUPS.StandardExampleApplyAttributes(RectangularBand, out, m, n);
  return out;
end);

# Rectangular band: constructors

InstallMethod(RectangularBandCons,
"for a filter and a positive integer and positive integer",
[IsTransformationSemigroup, IsPosInt, IsPosInt],
function(filter, m, n)
  local max, min, out, basic, im, i, j;

  max := Maximum(m, n);
  min := Minimum(m, n);
  out := EmptyPlist(max);

  basic := EmptyPlist(m * n + 1);
  for i in [0 .. m - 1] do
    for j in [1 .. n] do
      basic[n * i + j] := n * i + 1;
    od;
  od;

  if m = 1 then
    for i in [0 .. n - 1] do
      Add(out, Transformation(basic + i));
    od;
    return Semigroup(out);
  fi;

  for i in [0 .. min - 1] do
    im := Concatenation(basic + i, [i * (n + 1) + 1]);
    Add(out, Transformation(im));
  od;

  for i in [min .. m - 1] do
    Add(out, Transformation(Concatenation(basic, [i * n + 1])));
  od;

  for i in [min .. n - 1] do
    Add(out, Transformation(Concatenation(basic + i, [i + 1])));
  od;

  return Semigroup(out);
end);

#

InstallMethod(RectangularBandCons,
"for a filter and two positive integers",
[IsBipartitionSemigroup, IsPosInt, IsPosInt],
function(filter, m, n)
  local max, min, out, nrpoints, partitions, neg, i;

  max := Maximum(m, n);
  min := Minimum(m, n);
  out := EmptyPlist(max);

  # Find a small degree of partition monoid in which to embed rectangular band
  nrpoints := 1;
  while NrPartitionsSet([1 .. nrpoints]) < max do
    nrpoints := nrpoints + 1;
  od;

  partitions := PartitionsSet([1 .. nrpoints]);

  for i in [1 .. min] do
    Add(out, Bipartition(Concatenation(partitions[i], -1 * partitions[i])));
  od;

  neg := -1 * partitions[1];
  for i in [min + 1 .. m] do
    Add(out, Bipartition(Concatenation(partitions[i], neg)));
  od;

  for i in [min + 1 .. n] do
    Add(out, Bipartition(Concatenation(partitions[1], -1 * partitions[i])));
  od;

  return Semigroup(out);
end);

#

InstallMethod(RectangularBandCons,
"for a filter and a positive integer and positive integer",
[IsReesMatrixSemigroup, IsPosInt, IsPosInt],
function(filter, m, n)
  local id, mat;

  id := ();
  mat := List([1 .. n], x -> List([1 .. m], y -> id));
  return ReesMatrixSemigroup(Group(id), mat);
end);

# Rectangular band: other constructors

SEMIGROUPS.InstallConstructors2(RectangularBand,
                                [IsTransformationSemigroup,
                                 IsPartialPermSemigroup,
                                 IsBipartitionSemigroup,
                                 IsBlockBijectionSemigroup,
                                 IsReesMatrixSemigroup,
                                 IsReesZeroMatrixSemigroup],
                                [IsPosInt, IsPosInt]);

# Zero semigroup: main method

InstallGlobalFunction(ZeroSemigroup,
function(arg)
  local filter, n, out;

  if Length(arg) = 1  then
    filter := IsTransformationSemigroup;
    n := arg[1];
  elif Length(arg) = 2 then
    filter := arg[1];
    n := arg[2];
  fi;

  if not IsBound(n) or not IsPosInt(n) or not IsOperation(filter) then
    ErrorMayQuit("Semigroups: ZeroSemigroup: usage,\n",
                 "the arguments must be a positive integer or a filter and a ",
                 "positive integer,");
  fi;

  out := ZeroSemigroupCons(filter, n);
  if out = fail then
    ErrorMayQuit("Semigroups: ZeroSemigroup: usage,\n",
                 "the requested filter is not supported,");
  fi;
  SEMIGROUPS.StandardExampleApplyAttributes(ZeroSemigroup, out, n);
  return out;
end);

# Zero semigroup: constructors

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsTransformationSemigroup, IsPosInt],
function(filter, n)
  local zero, gens, out, i;

  if n = 1 then
    zero := Transformation([1]);
    gens := [zero];
  else
    zero := Transformation(List([1 .. 2 * n - 1], x -> 1));
    gens := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      gens[i] := Transformation(Concatenation([1 .. (2 * i) - 1] * 0 + 1,
                                              [2 * i + 1],
                                              [2 * i + 1 .. 2 * n - 1]
                                              * 0 + 1));
    od;
  fi;
  out := Semigroup(gens);
  SetMultiplicativeZero(out, zero);
  return out;
end);

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsPartialPermSemigroup, IsPosInt],
function(filter, n)
  local zero, gens, out, i;

  zero := PartialPerm([], []);
  if n = 1 then
    gens := [zero];
  else
    gens := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      gens[i] := PartialPerm([2 * i - 1], [2 * i]);
    od;
  fi;
  out := Semigroup(gens);
  SetMultiplicativeZero(out, zero);
  return out;
end);

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsBipartitionSemigroup, IsPosInt],
function(filter, n)
  local zero, out;

  if n = 2 then
    zero := Bipartition([[1], [2], [-1], [-2]]);
    out := Semigroup(Bipartition([[1, -2], [2], [-1]]));
    SetMultiplicativeZero(out, zero);
    return out;
  fi;
  return ZeroSemigroupCons(IsBlockBijectionSemigroup, n);
end);

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsBlockBijectionSemigroup, IsPosInt],
function(filter, n)
  local zero, gens, points, pair, out, i;

  if n = 1 then
    zero := Bipartition([[1, -1]]);
    gens := [zero];
  elif n = 2 then
    points := Concatenation([1 .. 3], [-3 .. -1]);
    zero := Bipartition([points]);
    gens := [Bipartition([[1, -2], [-1, 2, 3, -3]])];
  else
    points := Concatenation([1 .. 2 * (n - 1)], -[1 .. 2 * (n - 1)]);
    zero := Bipartition([points]);
    gens := EmptyPlist(n - 1);
    for i in [1 .. n - 1] do
      pair := [2 * i - 1, -(2 * i)];
      gens[i] := Bipartition([pair, Difference(points, pair)]);
    od;
  fi;
  out := Semigroup(gens);
  SetMultiplicativeZero(out, zero);
  return out;
end);

#

InstallMethod(ZeroSemigroupCons,
"for a filter and a positive integer",
[IsReesZeroMatrixSemigroup, IsPosInt],
function(filter, n)
  local mat;

  if n = 1 then
    ErrorMayQuit("Semigroups: ZeroSemigroupCons: usage:\n",
                 "there is no Rees 0-matrix semigroup of order 1,");
  fi;
  mat := [[1 .. n - 1] * 0];
  return ReesZeroMatrixSemigroup(Group(()), mat);
end);

# Zero semigroup: other constructors

SEMIGROUPS.InstallConstructors1(ZeroSemigroup,
                                [IsTransformationSemigroup,
                                 IsPartialPermSemigroup,
                                 IsBipartitionSemigroup,
                                 IsBlockBijectionSemigroup,
                                 IsReesMatrixSemigroup,
                                 IsReesZeroMatrixSemigroup],
                                [IsPosInt]);

# Left zero semigroup: main method

InstallGlobalFunction(LeftZeroSemigroup,
function(arg)
  local out;
  if Length(arg) = 2 and IsOperation(arg[1]) and IsPosInt(arg[2]) then
    out := RectangularBand(arg[1], arg[2], 1);
  elif Length(arg) = 1 and IsPosInt(arg[1]) then
    out := RectangularBand(IsTransformationSemigroup, arg[1], 1);
  else
    ErrorMayQuit("Semigroups: LeftZeroSemigroup: usage,\n",
                 "the arguments must be a positive integer or ",
                 "a filter and a positive integer,");
  fi;
  return out;
end);

# Right zero semigroup: main method

InstallGlobalFunction(RightZeroSemigroup,
function(arg)
  local out;
  if Length(arg) = 2 and IsOperation(arg[1]) and IsPosInt(arg[2]) then
    out := RectangularBand(arg[1], 1, arg[2]);
  elif Length(arg) = 1 and IsPosInt(arg[1]) then
    out := RectangularBand(IsTransformationSemigroup, 1, arg[1]);
  else
    ErrorMayQuit("Semigroups: RightZeroSemigroup: usage,\n",
                 "the arguments must be a positive integer or ",
                 "a filter and a positive integer,");
  fi;
  return out;
end);
