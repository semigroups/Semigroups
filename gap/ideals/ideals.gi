#############################################################################
##
#W  ideals.gi
#Y  Copyright (C) 2013-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for ideals of semigroups, which do not depend on
# the representation as IsActingSemigroup or IsEnumerableSemigroupRep.

InstallImmediateMethod(IsSemigroupIdeal, IsSemigroup, 0, IsMagmaIdeal);
InstallTrueMethod(IsSemigroupIdeal, IsMagmaIdeal and IsSemigroup);
InstallTrueMethod(IsSemigroup, IsSemigroupIdeal);

BindGlobal("_ViewStringForSemigroupsIdeals",
function(I)
  local str, suffix, nrgens;

  str := "\><";

  if HasIsCommutative(I) and IsCommutative(I) then
    Append(str, "\>commutative\< ");
  fi;

  if not IsGroup(I) then
    if HasIsTrivial(I) and IsTrivial(I) then
      # do nothing
    elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then
      Append(str, "\>0-simple\< ");
    elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then
      Append(str, "\>simple\< ");
    fi;

    if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then
      Append(str, "\>inverse\< ");
    elif HasIsRegularSemigroup(I)
        and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then
      if IsRegularSemigroup(I) then
        Append(str, "\>regular\< ");
      else
        Append(str, "\>non-regular\< ");
      fi;
    fi;
  fi;

  Append(str, SemigroupViewStringPrefix(I));

  Append(str, "\>semigroup\< \>ideal\< ");

  if HasIsTrivial(I) and not IsTrivial(I) and HasSize(I) then
    Append(str, "\>of size\> ");
    Append(str, ViewString(Size(I)));
    Append(str, ",\<\< ");
  fi;

  suffix := SemigroupViewStringSuffix(I);
  if suffix <> ""
      and not (HasIsTrivial(I) and not IsTrivial(I) and HasSize(I)) then
    suffix := Concatenation("of ", suffix);
  fi;
  Append(str, suffix);

  nrgens := Length(GeneratorsOfSemigroupIdeal(I));

  Append(str, "with\> ");
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens > 1 or nrgens = 0 then
    Append(str, "s\<");
  else
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

InstallMethod(ViewString,
"for a semigroup ideal with ideal generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
1, #to beat the library method
_ViewStringForSemigroupsIdeals);

InstallMethod(ViewString,
"for a semigroup ideal with ideal generators",
[IsPartialPermSemigroup and IsInverseSemigroup and IsSemigroupIdeal and
 HasGeneratorsOfSemigroupIdeal], 1, #to beat the library method
_ViewStringForSemigroupsIdeals);

InstallMethod(ViewString,
"for a semigroup ideal with ideal generators",
[IsPartialPermMonoid and IsInverseMonoid and IsSemigroupIdeal and
 HasGeneratorsOfSemigroupIdeal], 1, #to beat the library method
_ViewStringForSemigroupsIdeals);

MakeReadWriteGlobal("_ViewStringForSemigroupsIdeals");
Unbind(_ViewStringForSemigroupsIdeals);

InstallMethod(PrintObj,
"for a semigroup ideal with ideal generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  Print(PrintString(I));
end);

InstallMethod(PrintString,
"for a semigroup ideal with ideal generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local str;

  str := "\>\>SemigroupIdeal(\< \>";
  Append(str, PrintString(SupersemigroupOfIdeal(I)));
  Append(str, ",\< \>");
  Append(str, PrintString(GeneratorsOfSemigroupIdeal(I)));
  Append(str, "\< )\<");
  return str;
end);

# This is required since there is a method for ViewObj of a semigroup ideal
# with a higher rank than the default method which delegates from ViewObj to
# ViewString. Hence the method for ViewString is never invoked without the
# method below.

InstallMethod(ViewObj,
"for a semigroup ideal with ideal generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal], 1,
function(I)
  Print(ViewString(I));
end);

InstallMethod(\., "for a semigroup ideal with generators and pos int",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal, IsPosInt],
function(S, n)
  S := GeneratorsOfSemigroupIdeal(S);
  n := NameRNam(n);
  n := Int(n);
  if n = fail or Length(S) < n then
    ErrorNoReturn("Semigroups: \\.: usage,\n",
                  "the second argument <n> should be a positive integer\n",
                  "not greater than the number of generators of the semigroup ",
                  "<S> in\nthe first argument,");
  fi;
  return S[n];
end);

InstallMethod(\=, "for semigroup ideals",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal,
 IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I, J)

  if SupersemigroupOfIdeal(I) = SupersemigroupOfIdeal(J) then
    return ForAll(GeneratorsOfMagmaIdeal(I), x -> x in J)
      and ForAll(GeneratorsOfMagmaIdeal(J), x -> x in I);
  fi;
  return ForAll(GeneratorsOfSemigroup(I), x -> x in J)
    and ForAll(GeneratorsOfSemigroup(J), x -> x in I);
end);

InstallMethod(\=, "for a semigroup ideal and semigroup with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal,
 IsSemigroup and HasGeneratorsOfSemigroup],
function(I, S)
  if ForAll(GeneratorsOfSemigroup(S), x -> x in I) then
    if S = Parent(I) then
      return true;
    elif HasGeneratorsOfSemigroup(I) then
      return ForAll(GeneratorsOfSemigroup(I), x -> x in S);
    else
      return Size(I) = Size(S);
    fi;
  fi;
  return false;
end);

InstallMethod(\=, "for a semigroup with generators and a semigroup ideal",
[IsSemigroup and HasGeneratorsOfSemigroup,
 IsSemigroupIdeal and HasGeneratorsOfMagmaIdeal],
function(S, I)
  return I = S;
end);

InstallMethod(Representative, "for a semigroup ideal",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  return Representative(GeneratorsOfMagmaIdeal(I));
end);

# a convenience, similar to the functions <Semigroup>, <Monoid>, etc

InstallGlobalFunction(SemigroupIdeal,
function(arg)
  local out, i;

  if Length(arg) = 0 then
    # no argument given, error
    ErrorNoReturn("Semigroups: SemigroupIdeal: usage,\n",
                  "the second argument must be a combination ",
                  "of generators,\nlists of generators, or semigroups,");
  elif not IsSemigroup(arg[1]) then
    ErrorNoReturn("Semigroups: SemigroupIdeal: usage,\n",
                  "the first argument must be a semigroup,");
  elif Length(arg) = 1 then
    ErrorNoReturn("Semigroups: SemigroupIdeal: usage,\n",
                  "there must be a second argument, which specifies\n",
                  "the generators of the ideal,");
  elif Length(arg) = 2 and IsMatrix(arg[2]) then
    # special case for matrices, because they may look like lists
    return SemigroupIdealByGenerators(arg[1], [arg[2]]);

  elif Length(arg) = 2 and IsList(arg[2]) and 0 < Length(arg[2]) then
    # list of generators
    return SemigroupIdealByGenerators(arg[1], arg[2]);

  elif (IsMultiplicativeElement(arg[2])
        and IsGeneratorsOfSemigroup([arg[2]]))
      or (IsMultiplicativeElementCollection(arg[2])
          and IsGeneratorsOfSemigroup(arg[2]))
      or (HasIsEmpty(arg[2]) and IsEmpty(arg[2])) then
    # generators and collections of generators
    out := [];
    for i in [2 .. Length(arg)] do
      #so that we can pass the options record in the Semigroups package
      if i = Length(arg) and IsRecord(arg[i]) then
        return SemigroupIdealByGenerators(arg[1], out, arg[i]);
      elif IsMultiplicativeElement(arg[i]) and
          IsGeneratorsOfSemigroup([arg[i]]) then
        Add(out, arg[i]);
      elif IsGeneratorsOfSemigroup(arg[i]) then
        if HasGeneratorsOfSemigroupIdeal(arg[i]) then
          Append(out, GeneratorsOfSemigroupIdeal(arg[i]));
        elif HasGeneratorsOfSemigroup(arg[i]) then
          Append(out, GeneratorsOfSemigroup(arg[i]));
        elif IsList(arg[i]) then
          Append(out, arg[i]);
        else
          Append(out, AsList(arg[i]));
        fi;
      else
        ErrorNoReturn("Semigroups: SemigroupIdeal: usage,\n",
                      "the second argument must be a ",
                      "combination of generators,\n lists of generators, ",
                      "or semigroups,");
      fi;
    od;
    return SemigroupIdealByGenerators(arg[1], out);
  fi;
  ErrorNoReturn("Semigroups: SemigroupIdeal: usage,\n",
                "the second argument must be a combination ",
                "of generators,\nlists of generators, or semigroups,");
end);

InstallMethod(SemigroupIdealByGenerators,
"for a semigroup, multiplicative element collection",
[IsSemigroup, IsMultiplicativeElementCollection],
function(S, gens)
  return SemigroupIdealByGenerators(S, gens, SEMIGROUPS.OptionsRec(S));
end);

InstallMethod(SemigroupIdealByGenerators,
"for semigroup, multiplicative element collection, and record",
[IsSemigroup, IsMultiplicativeElementCollection, IsRecord],
function(S, gens, opts)
  if not ForAll(gens, x -> x in S) then
    ErrorNoReturn("Semigroups: SemigroupIdealByGenerators: usage,\n",
                  "the second argument <gens> do not all belong to the ",
                  "semigroup,");
  fi;
  return SemigroupIdealByGeneratorsNC(S, gens, opts);
end);

InstallMethod(SemigroupIdealByGeneratorsNC,
"for semigroup, multiplicative element collection, and record",
[IsSemigroup, IsMultiplicativeElementCollection, IsRecord],
function(S, gens, opts)
  local filts, I;
  opts := SEMIGROUPS.ProcessOptionsRec(SEMIGROUPS.DefaultOptionsRec, opts);
  gens := AsList(gens);

  filts := IsMagmaIdeal and IsAttributeStoringRep;

  if opts.acting
      and (IsActingSemigroup(S) or IsGeneratorsOfActingSemigroup(gens)) then
    filts := filts and IsActingSemigroup;
    if opts.regular then
      filts := filts and IsRegularActingSemigroupRep;
    fi;
  elif IsEnumerableSemigroupRep(S)
      and IsGeneratorsOfEnumerableSemigroup(gens) then
    filts := filts and IsEnumerableSemigroupRep;
  fi;

  I := Objectify(NewType(FamilyObj(gens), filts), rec(opts := opts));

  if IsInverseActingSemigroupRep(S) then
    SetFilterObj(I, IsInverseActingSemigroupRep);
  elif IsRegularActingSemigroupRep(S) then
    SetFilterObj(I, IsRegularActingSemigroupRep);
  fi;

  if (HasIsInverseSemigroup(S) and IsInverseSemigroup(S)) then
    SetIsInverseSemigroup(I, true);
    if HasIsGeneratorsOfInverseSemigroup(S) then
      SetIsGeneratorsOfInverseSemigroup(I, IsGeneratorsOfInverseSemigroup(S));
    fi;
  elif (HasIsRegularSemigroup(S) and IsRegularSemigroup(S)) then
    SetIsRegularSemigroup(I, true);
  fi;

  if (HasIsStarSemigroup(S) and IsStarSemigroup(S)) then
    SetIsStarSemigroup(I, true);
  fi;

  if (HasIsRegularSemigroup(S) and not IsRegularSemigroup(S)) then
    # <S> is a non-regular semigroup or ideal
    SetSupersemigroupOfIdeal(I, S);
  elif HasSupersemigroupOfIdeal(S) then
    # <S> is a regular ideal

    # this takes precedence over the last case since we hope that the
    # supersemigroup of an ideal has fewer generators than the ideal...
    SetSupersemigroupOfIdeal(I, SupersemigroupOfIdeal(S));
  else
    # <S> is a regular semigroup
    SetSupersemigroupOfIdeal(I, S);
  fi;

  SetParent(I, S);
  SetGeneratorsOfMagmaIdeal(I, gens);

  if not IsActingSemigroup(I) then # to keep the craziness in the library happy!
    SetActingDomain(I, S);
  elif IsActingSemigroup(I)
      and not (HasIsRegularSemigroup(I) and IsRegularSemigroup(I)) then
    # this is done so that the ideal knows it is regular or non-regular from
    # the point of creation...
    Enumerate(SemigroupIdealData(I), infinity, ReturnFalse);
  fi;

  return I;
end);

# JDM HERE!!
InstallMethod(MinimalIdealGeneratingSet,
"for a semigroup ideal with generators",
[IsSemigroupIdeal and HasGeneratorsOfSemigroupIdeal],
function(I)
  local S, dclasses, gr, labels, x;

  if Length(GeneratorsOfSemigroupIdeal(I)) = 1 then
    return GeneratorsOfSemigroupIdeal(I);
  fi;

  S := SupersemigroupOfIdeal(I);
  dclasses := [];
  for x in GeneratorsOfSemigroupIdeal(I) do
    if not ForAny(dclasses, D -> x in D) then
      Add(dclasses, DClass(S, x));
    fi;
  od;
  # TODO improve the following
  gr := InducedSubdigraph(Digraph(PartialOrderOfDClasses(S)),
                          List(dclasses, x -> Position(DClasses(S), x)));
  gr := DigraphRemoveLoops(gr);
  labels := DigraphVertexLabels(gr);
  return List(DigraphSources(gr), x -> Representative(DClasses(S)[labels[x]]));
end);

# JDM: is there a better method? Certainly for regular acting ideals
# there is

InstallMethod(InversesOfSemigroupElementNC,
"for a semigroup ideal and multiplicative element",
[IsSemigroupIdeal, IsMultiplicativeElement],
function(I, x)
  return InversesOfSemigroupElementNC(SupersemigroupOfIdeal(I), x);
end);

InstallMethod(IsCommutativeSemigroup, "for a semigroup ideal",
[IsSemigroupIdeal],
function(I)
  local x, y;

  if HasParent(I) and HasIsCommutativeSemigroup(Parent(I))
      and IsCommutativeSemigroup(Parent(I)) then
    return true;
  fi;

  for x in GeneratorsOfSemigroupIdeal(I) do
    for y in GeneratorsOfSemigroup(SupersemigroupOfIdeal(I)) do
      if not x * y = y * x then
        return false;
      fi;
    od;
  od;

  return true;
end);

InstallMethod(IsTrivial, "for a semigroup ideal",
[IsSemigroupIdeal],
function(I)
  local gens;

  if HasIsTrivial(Parent(I)) and IsTrivial(Parent(I)) then
    return true;
  fi;

  gens := GeneratorsOfSemigroupIdeal(I);
  return MultiplicativeZero(I) = gens[1] and ForAll(gens, x -> gens[1] = x);
end);

InstallMethod(IsFactorisableInverseMonoid, "for an inverse semigroup ideal",
[IsSemigroupIdeal and IsInverseSemigroup],
function(I)

  if I = SupersemigroupOfIdeal(I) then
    return IsFactorisableInverseMonoid(SupersemigroupOfIdeal(I));
  fi;
  return false;
end);
