#############################################################################
##
#W  enums.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# No attempt has been made to get good test coverage for this file, since it
# will hopefully be binned in the near future.

# technical...

# <convert_in> must return <fail> if it is not possible to convert
# <convert_out> must check if its argument is <fail> and if it is, then it
# should return <fail>, <convert_out> should have two arguments <enum> and <nr>
# where <nr> refers to the position in <baseenum>.

BindGlobal("EnumeratorByEnumerator",
function(obj, baseenum, convert_out, convert_in, filts, record)
  local enum, filt;

  if not (IsDomain(obj) or IsCollectionFamily(obj)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumerator: usage,\n",
                  "the first argument <obj> must be a domain or a collections",
                  "family,");
  elif not (IsEnumeratorByFunctions(baseenum) or IsList(baseenum)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumerator: usage,\n",
                  "the second argument <baseenum> must be an enumerator or a ",
                  "list,");
  elif not (IsFunction(convert_out) and IsFunction(convert_in)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumerator: usage,\n",
                  "the third and fourth arguments <convert_out> and ",
                  "<convert_in>\nmust be functions,");
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumerator: usage,\n",
                  "the fifth argument <filts> must be a list of filters,");
  elif not (IsRecord(record) and IsMutable(record))
      or IsBound(record.baseenum)
      or IsBound(record.convert_out)
      or IsBound(record.convert_in)
      or IsBound(record.NumberElement)
      or IsBound(record.ElementNumber) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumerator: usage,\n",
                  "the sixth argument <record> must be a mutable record",
                  "with no components\n",
                  "named:\n`baseenum', `convert_out', `convert_in',",
                  "`ElementNumber',\n or `NumberElement',");
  fi;

  record.baseenum := baseenum;
  record.convert_out := convert_out;
  record.convert_in := convert_in;

  #
  record.NumberElement := function(enum, elt)
    local converted;
    converted := enum!.convert_in(enum, elt);
    if converted = fail then
      return fail;
    fi;
    return Position(enum!.baseenum, converted);
  end;
  #
  record.ElementNumber := function(enum, nr)
    return enum!.convert_out(enum, enum!.baseenum[nr]);
  end;
  #
  if not IsBound(record.Length) then
    record.Length := enum -> Length(enum!.baseenum);
  fi;
  #
  if IsEnumeratorByFunctions(baseenum) then

    if IsBound(baseenum!.Membership) then
      record.Membership := function(enum, elt)
        local converted;
        converted := enum!.convert_in(enum, elt);
        if converted = fail then
          return false;
        fi;
        return enum!.convert_in(enum, elt) in enum!.baseenum;
      end;
    fi;

    if IsBound(baseenum!.IsBound\[\]) then
      record.IsBound\[\] := function(enum, nr)
        return IsBound(enum!.baseenum[nr]);
      end;
    fi;

  fi;
  #
  enum := EnumeratorByFunctions(obj, record);

  for filt in filts do #filters
    SetFilterObj(enum, filt);
  od;
  return enum;
end);

BindGlobal("EnumeratorByEnumOfEnums",
function(obj, record, baseenum, convert, filts)
  local enum, filt;

  if not (IsDomain(obj) or IsCollectionFamily(obj)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumOfEnums: usage,\n",
                  "the first argument <obj> must be a domain or a collections ",
                  "family,");
  elif not IsRecord(record)
      or IsBound(record.ElementNumber)
      or IsBound(record.NumberElement)
      or IsBound(record.baseenum)
      or IsBound(record.enumofenums) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumOfEnums: usage,\n",
                  "the second argument  <record> must be a record",
                  "with no components named:\n",
                  "`NumberElement', `ElementNumber', `baseenum', or ",
                  "`enumofenums',");
    # TODO add check for third arg
  elif not IsFunction(convert) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumOfEnums: usage,\n",
                  "the fourth argument <convert> must be a function,");
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then
    ErrorNoReturn("Semigroups: EnumeratorByEnumOfEnums: usage,\n",
                  "the fifth argument <filts> must be a list of filters,");
  fi;

  record.baseenum := baseenum;
  record.enumofenums := EmptyPlist(Length(baseenum));

  if not IsBound(record.Length) then #maybe a better way is in record.Length...
    record.Length := function(enum)
      local tot, enumofenums, baseenum, i;
      tot := 0;
      enumofenums := enum!.enumofenums;
      baseenum := enum!.baseenum;
      for i in [1 .. Length(baseenum)] do
        if not IsBound(enumofenums[i]) then
          enumofenums[i] := Enumerator(baseenum[i]);
        fi;
        tot := tot + Length(enumofenums[i]);
      od;
      return tot;
    end;
  fi;
  #
  record.ElementNumber := function(enum, pos)
    local enumofenums, baseenum, i;
    if pos > Length(enum) then
      return fail;
    fi;
    enumofenums := enum!.enumofenums;
    baseenum := enum!.baseenum;

    i := 1;
    if not IsBound(enumofenums[1]) then
      enumofenums[1] := Enumerator(baseenum[1]);
    fi;

    while pos > Length(enumofenums[i]) do
      pos := pos - Length(enumofenums[i]);
      i := i + 1;
      if not IsBound(enumofenums[i]) then
        enumofenums[i] := Enumerator(baseenum[i]);
      fi;
    od;
    return enumofenums[i][pos];
  end;
  #
  record.NumberElement := function(enum, elt)
    local baseenum, enumofenums, conv, basepos, pos, i;

    baseenum := enum!.baseenum;
    enumofenums := enum!.enumofenums;
    conv := convert(enum, elt);
    if conv = fail then
      return fail;
    fi;
    basepos := Position(baseenum, conv);
    if basepos = fail then
      return fail;
    fi;
    if not IsBound(enumofenums[basepos]) then
      enumofenums[basepos] := Enumerator(baseenum[basepos]);
    fi;
    pos := Position(enumofenums[basepos], elt);
    if pos = fail then
      return fail;
    fi;
    for i in [1 .. basepos - 1] do
      if not IsBound(enumofenums[i]) then
        enumofenums[i] := Enumerator(baseenum[i]);
      fi;
      pos := pos + Length(enumofenums[i]);
    od;
    return pos;
  end;
  #
  enum := EnumeratorByFunctions(obj, record);
  for filt in filts do
    SetFilterObj(enum, filt);
  od;
  return enum;
end);

# the actual methods used...

# same method for regular/inverse,

# also this has really awful performance
# TODO write an improved version for enumerator sorted

InstallMethod(Enumerator, "for an acting semigroup",
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
function(s)
  local record, convert;

  if HasAsSSortedList(s) then
    return AsSSortedList(s);
  fi;

  record := rec();
  record.Length := x -> Size(s);
  record.Membership := function(x, enum)
    return x in enum!.parent;
  end;
  record.parent := s;

  convert := function(enum, x)
    if x in enum!.parent then
      return GreensRClassOfElementNC(enum!.parent, x);
    fi;
    return fail;
  end;

  return EnumeratorByEnumOfEnums(s, record, EnumeratorOfRClasses(s),
                                 convert, []);
end);

# same method for regular/inverse

#FIXME this should be improved, using Iterator for a regular or inverse
#semigroup, invokes IteratorOfRClassData which repeatedly recomputes the graded
#lambda orbs of the R-class reps.

InstallMethod(EnumeratorSorted, "for an acting semigroup",
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
S -> Immutable(SSortedList(ListIterator(Iterator(S), Size(S)))));

# different method for regular/inverse

# this could be better at the cost of being much more complicated...

InstallMethod(Enumerator, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert;

  if HasAsSSortedList(d) then
    return AsSSortedList(d);
  fi;
  record := rec(parent := d);

  record.PrintObj := function(enum)
    Print("<enumerator of D-class>");
  end;

  convert := function(enum, elt)
    return GreensRClassOfElement(enum!.parent, elt);
  end;

  return EnumeratorByEnumOfEnums(d, record, GreensRClasses(d),
                                 convert, []);
end);

# different method for inverse

InstallMethod(Enumerator, "for a regular D-class of an acting semigroup",
[IsRegularDClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, rho_scc, lambda_scc;

  if HasAsSSortedList(d) then
    return AsSSortedList(d);
  fi;

  Enumerate(LambdaOrb(d), infinity);
  Enumerate(RhoOrb(d), infinity);

  record := rec(parent := d);

  record.Membership := function(elt, enum)
    return elt in d;
  end;

  record.PrintObj := enum -> Print("<enumerator of D-class>");

  #
  convert_out := function(enum, tuple)
    local d, rep, act;
    if tuple = fail then
      return fail;
    fi;
    d := enum!.parent;
    rep := Representative(d);
    act := StabilizerAction(Parent(d));
    return act(RhoOrbMult(RhoOrb(d), RhoOrbSCCIndex(d),
               tuple[1])[1] * rep, tuple[2])
           * LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[3])[1];
  end;
  #
  convert_in := function(enum, elt)
    local d, s, k, l, f;

    d := enum!.parent;
    s := Parent(d);

    k := Position(RhoOrb(d), RhoFunc(s)(elt));
    if OrbSCCLookup(RhoOrb(d))[k] <> RhoOrbSCCIndex(d) then
      return fail;
    fi;
    l := Position(LambdaOrb(d), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[l] <> LambdaOrbSCCIndex(d) then
      return fail;
    fi;

    f := RhoOrbMult(RhoOrb(d), RhoOrbSCCIndex(d), k)[2] * elt
     * LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), l)[2];

    return [k, LambdaPerm(s)(Representative(d), f), l];
  end;
  #
  rho_scc := OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
  lambda_scc := OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];

  return EnumeratorByEnumerator(d,
           EnumeratorOfCartesianProduct(rho_scc,
                                        SchutzenbergerGroup(d),
                                        lambda_scc),
           convert_out, convert_in, [], record);
end);

InstallMethod(Enumerator, "for a D-class of an inverse acting semigroup",
[IsGreensDClass and IsInverseActingRepGreensClass
 and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, lambda_scc;

  if HasAsSSortedList(d) then
    return AsSSortedList(d);
  fi;

  Enumerate(LambdaOrb(d), infinity);

  record := rec(parent := d);
  record.PrintObj := function(enum)
    Print("<enumerator of D-class>");
  end;

  #
  convert_out := function(enum, tuple)
    local d, rep, act;
    if tuple = fail then
      return fail;
    fi;
    d := enum!.parent;
    rep := Representative(d);
    act := StabilizerAction(Parent(d));
    return act(LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[1])[2]
               * rep, tuple[2])
               * LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d),
                               tuple[3])[1];
  end;
  #
  convert_in := function(enum, elt)
    local d, s, k, l, f;

    d := enum!.parent;
    s := Parent(d);

    k := Position(LambdaOrb(d), RhoFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[k] <> LambdaOrbSCCIndex(d) then
      return fail;
    fi;
    l := Position(LambdaOrb(d), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[l] <> LambdaOrbSCCIndex(d) then
      return fail;
    fi;

    f := LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), k)[1] * elt
     * LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), l)[2];

    return [k, LambdaPerm(s)(Representative(d), f), l];
  end;
  #
  lambda_scc := OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
  return EnumeratorByEnumerator(d,
     EnumeratorOfCartesianProduct(lambda_scc,
                                  SchutzenbergerGroup(d),
                                  lambda_scc),
     convert_out, convert_in, [], record);
end);

# same method for inverse/regular

InstallMethod(Enumerator, "for H-class of acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  if HasAsSSortedList(h) then
    return AsSSortedList(h);
  fi;
  return EnumeratorByFunctions(h,
    rec(schutz := Enumerator(SchutzenbergerGroup(h)),

        ElementNumber := function(enum, pos)
          if pos > Length(enum) then
            return fail;
          fi;

          return StabilizerAction(Parent(h))(Representative(h),
                                             enum!.schutz[pos]);
        end,

        NumberElement := function(enum, f)
          local s, rep;
          s := Parent(h);
          rep := Representative(h);

          if LambdaFunc(s)(f) <> LambdaFunc(s)(rep)
              or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
            return fail;
          fi;

          return Position(enum!.schutz, LambdaPerm(s)(rep, f));
        end,

        Membership := function(elm, enum)
          return elm in h; #the H-class itself!
        end,

        Length := enum -> Size(h),

        PrintObj := function(enum)
          Print("<enumerator of H-class>");
        end));
end);

# same method for regular, different method for inverse

InstallMethod(Enumerator, "for L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local record, convert_out, convert_in, scc;

  if HasAsSSortedList(l) then
    return AsSSortedList(l);
  fi;

  record := rec(parent := l);
  record.Membership := function(elm, enum)
    return elm in l;
  end;
  record.PrintObj := function(enum)
    Print("<enumerator of L-class>");
  end;

  record.Length := enum -> Size(l);
  #
  convert_out := function(enum, tuple)
    local l, rep, act;
    if tuple = fail then
      return fail;
    fi;
    l := enum!.parent;
    rep := Representative(l);
    act := StabilizerAction(Parent(l));
    return act(RhoOrbMult(RhoOrb(l), RhoOrbSCCIndex(l), tuple[1])[1]
               * rep, tuple[2]);
  end;
  #
  convert_in := function(enum, elt)
    local l, s, i, f;
    l := enum!.parent;
    s := Parent(l);

    if LambdaFunc(s)(elt) <> LambdaFunc(s)(Representative(l)) then
      return fail;
    fi;

    i := Position(RhoOrb(l), RhoFunc(s)(elt));
    if OrbSCCLookup(RhoOrb(l))[i] <> RhoOrbSCCIndex(l) then
      return fail;
    fi;

    f := RhoOrbMult(RhoOrb(l), RhoOrbSCCIndex(l), i)[2] * elt;

    return [i, LambdaPerm(s)(Representative(l), f)];
  end;
  #
  scc := OrbSCC(RhoOrb(l))[RhoOrbSCCIndex(l)];
  return EnumeratorByEnumerator(l,
    EnumeratorOfCartesianProduct(scc,
                                 SchutzenbergerGroup(l)),
    convert_out, convert_in, [], record);
end);

InstallMethod(Enumerator,
"for L-class of an inverse acting semigroup rep",
[IsGreensLClass and IsInverseActingRepGreensClass
 and IsActingSemigroupGreensClass],
function(l)
  local record, convert_out, convert_in, scc;

  if HasAsSSortedList(l) then
    return AsSSortedList(l);
  fi;

  record := rec(parent := l);
  record.Membership := function(elm, enum)
    return elm in l;
  end;
  record.PrintObj := function(enum)
    Print("<enumerator of L-class>");
  end;

  record.Length := enum -> Size(l);
  #
  convert_out := function(enum, tuple)
    local l, rep, act;
    if tuple = fail then
      return fail;
    fi;
    l := enum!.parent;
    rep := Representative(l);
    act := StabilizerAction(Parent(l));
    return act(LambdaOrbMult(LambdaOrb(l), LambdaOrbSCCIndex(l), tuple[1])[2]
               * rep, tuple[2]);
  end;
  #
  convert_in := function(enum, elt)
    local l, s, i, f;
    l := enum!.parent;
    s := Parent(l);

    if LambdaFunc(s)(elt) <> LambdaFunc(s)(Representative(l)) then
      return fail;
    fi;

    i := Position(LambdaOrb(l), RhoFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(l))[i] <> LambdaOrbSCCIndex(l) then
      return fail;
    fi;

    f := LambdaOrbMult(LambdaOrb(l), LambdaOrbSCCIndex(l), i)[1] * elt;

    return [i, LambdaPerm(s)(Representative(l), f)];
  end;
  #
  scc := OrbSCC(LambdaOrb(l))[LambdaOrbSCCIndex(l)];
  return EnumeratorByEnumerator(l,
           EnumeratorOfCartesianProduct(scc,
                                        SchutzenbergerGroup(l)),
           convert_out, convert_in, [], record);
end);

# same method for regular/inverse

InstallMethod(Enumerator, "for R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local record, convert_out, convert_in, scc;

  if HasAsSSortedList(r) then
    return AsSSortedList(r);
  fi;

  record := rec(parent := r);
  record.Membership := function(elm, enum)
    return elm in r;
  end;
  record.PrintObj := function(enum)
    Print("<enumerator of R-class>");
  end;

  record.Length := enum -> Size(r);
  #
  convert_out := function(enum, tuple)
    local r, rep;
    if tuple = fail then
      return fail;
    fi;
    r := enum!.parent;
    rep := Representative(r);
    return StabilizerAction(Parent(r))(rep, tuple[1])
     * LambdaOrbMult(LambdaOrb(r), LambdaOrbSCCIndex(r), tuple[2])[1];
  end;
  #
  convert_in := function(enum, elt)
    local r, s, i, f;
    r := enum!.parent;
    s := Parent(r);

    if RhoFunc(s)(elt) <> RhoFunc(s)(Representative(r)) then
      return fail;
    fi;

    i := Position(LambdaOrb(r), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(r))[i] <> LambdaOrbSCCIndex(r) then
      return fail;
    fi;

    f := elt * LambdaOrbMult(LambdaOrb(r), LambdaOrbSCCIndex(r), i)[2];

    return [LambdaPerm(s)(Representative(r), f), i];
  end;
  #
  scc := OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)];
  return EnumeratorByEnumerator(r,
           EnumeratorOfCartesianProduct(SchutzenbergerGroup(r),
                                        scc),
           convert_out, convert_in, [], record);
end);

InstallGlobalFunction(NumberArrangement,
function(arr, n)
  local bool, m, mult, factor, out, k, i, j;

  if IsEmpty(arr) then
    return 1;
  fi;

  bool := BlistList([1 .. n], []);
  m := Length(arr);

  bool[arr[1]] := true;
  mult := Product([n - m + 1 .. n - 1]);
  factor := n;
  out := (arr[1] - 1) * mult;

  for i in [2 .. m] do
    k := 0;
    for j in [1 .. arr[i] - 1] do
      if not bool[j] then
        k := k + 1;
      fi;
    od;
    bool[arr[i]] := true;
    factor := factor - 1;
    mult := mult / factor;
    out := out + k * mult;
  od;
  return out + 1;
end);

InstallGlobalFunction(ArrangementNumber,
function(r, m, n)
  local bool, mult, factor, q, out, j, k, i;
  if m = 0 then
    return [];
  fi;
  if m = 1 then
    return [r];
  fi;
  bool := BlistList([1 .. n], []);
  r := r - 1;
  mult := Product([n - m + 1 .. n - 1]);
  factor := n;
  q := QuotientRemainder(r, mult);
  out := [q[1] + 1];
  bool[q[1] + 1] := true;

  for i in [2 .. m - 1] do
    factor := factor - 1;
    mult := mult / factor;
    q := QuotientRemainder(q[2], mult);
    j := 0;
    k := 0;
    repeat
      j := j + 1;
      if not bool[j] then
        k := k + 1;
      fi;
    until k = q[1] + 1;
    bool[j] := true;
    out[i] := j;
  od;
  j := 0;
  k := 0;
  repeat
    j := j + 1;
    if not bool[j] then
      k := k + 1;
    fi;
  until k = q[2] + 1;
  out[m] := j;
  return out;
end);

InstallGlobalFunction(EnumeratorOfArrangements,
function(m, n)
  local convert_out, convert_in, fam;

  if not IsPosInt(n) then
    ErrorNoReturn("Semigroups: EnumeratorOfArrangements: usage,\n",
                  "the second argument <n> must be a positive integer,");
  elif not (IsInt(m) and m >= 0) then
    ErrorNoReturn("Semigroups: EnumeratorOfArrangements: usage,\n",
                  "the first argument <m> must be a non-negative integer,");
  elif m > n then
    ErrorNoReturn("Semigroups: EnumeratorOfArrangements: usage,\n",
                  "the first argument <m> must be no greater than the\n",
                  "second argument <n>,");
  fi;

  convert_out := function(enum, x)
    return ArrangementNumber(x, m, n);
  end;

  convert_in := function(enum, x)
    return NumberArrangement(x, n);
  end;

  fam := CollectionsFamily(FamilyObj(ArrangementNumber(1, m, n)));

  return EnumeratorByEnumerator(fam,
                                Enumerator([1 .. NrArrangements([1 .. n], m)]),
                                convert_out, convert_in, [], rec());
end);
