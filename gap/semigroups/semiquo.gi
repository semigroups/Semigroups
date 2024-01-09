#############################################################################
##
##  semigroups/semiquo.gi
##  Copyright (C) 2014-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(ViewObj, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  Print("<quotient of ");
  ViewObj(QuotientSemigroupCongruence(S));
  Print(">");
end);

InstallMethod(OneImmutable, "for a quotient semigroup",
[IsQuotientSemigroup],
S -> One(QuotientSemigroupPreimage(S)) ^ QuotientSemigroupHomomorphism(S));

InstallMethod(GeneratorsOfSemigroup, "for a quotient semigroup",
[IsQuotientSemigroup],
function(S)
  local T;
  T := QuotientSemigroupPreimage(S);
  return DuplicateFreeList(Images(QuotientSemigroupHomomorphism(S),
                                  GeneratorsOfSemigroup(T)));
end);

InstallMethod(\*, "for multiplicative coll coll and congruence class",
[IsMultiplicativeElementCollColl, IsCongruenceClass],
function(list, nonlist)
  if ForAll(list, IsCongruenceClass) then
    return PROD_LIST_SCL_DEFAULT(list, nonlist);
  fi;
  TryNextMethod();
end);

InstallMethod(\*, "for congruence class and multiplicative coll coll",
[IsCongruenceClass, IsMultiplicativeElementCollColl],
function(nonlist, list)
  if ForAll(list, IsCongruenceClass) then
    return PROD_SCL_LIST_DEFAULT(nonlist, list);
  fi;
  TryNextMethod();
end);

InstallMethod(\/, "for a semigroup and an ideal",
[IsSemigroup, IsSemigroupIdeal],
{S, I} -> S / ReesCongruenceOfSemigroupIdeal(I));

# The next function is copied (almost) verbatim from the GAP library (4.11) so
# that the QuotientSemigroupHomomorphism is a homomorphism object.
MakeReadWriteGlobal("HomomorphismQuotientSemigroup");
UnbindGlobal("HomomorphismQuotientSemigroup");

DeclareGlobalFunction("HomomorphismQuotientSemigroup");

InstallGlobalFunction(HomomorphismQuotientSemigroup,
function(cong)
  local S, Qrep, efam, filters, Q, hom, Qgens;

    if not IsSemigroupCongruence(cong) then
      ErrorNoReturn("the argument should be a semigroup congruence");
    fi;
    S := Source(cong);
    Qrep := EquivalenceClassOfElementNC(cong, Representative(S));
    efam := FamilyObj(Qrep);
    filters := IsSemigroup and IsQuotientSemigroup and IsAttributeStoringRep;
    if IsMonoid(S) then
      filters := filters and IsMagmaWithOne;
    fi;
    if HasIsFinite(S) and IsFinite(S) then
      filters := filters and IsFinite;
    fi;
    Q := Objectify(NewType(CollectionsFamily(efam), filters),
                   rec());
    SetRepresentative(Q, Qrep);
    SetQuotientSemigroupPreimage(Q, S);
    SetQuotientSemigroupCongruence(Q, cong);
    hom := SemigroupHomomorphismByFunctionNC
            (S, Q, x -> EquivalenceClassOfElementNC(cong, x));
    SetQuotientSemigroupHomomorphism(Q, hom);
    efam!.quotient := Q;
    if IsMonoid(Q) and HasOne(S) then
      SetOne(Q, One(S) ^ QuotientSemigroupHomomorphism(Q));
    fi;
    if HasGeneratorsOfMagma(S) or HasGeneratorsOfMagmaWithInverses(S)
          or HasGeneratorsOfSemigroup(S) then
      Qgens := List(GeneratorsOfSemigroup(S),
                    s -> s ^ QuotientSemigroupHomomorphism(Q));
      SetGeneratorsOfSemigroup(Q, Qgens);
    fi;
    return QuotientSemigroupHomomorphism(Q);
end);
