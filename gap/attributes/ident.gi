#############################################################################
##
#W  ident.gi
#Y  Copyright (C) 2017                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(VerifyIdentity,
"for a mult. element coll. and a homogeneous list",
[IsMultiplicativeElementCollection, IsHomogeneousList],
function(coll, id)
  local lhs, rhs, nr_letters, tup;

  if IsEmpty(id) then
    ErrorNoReturn("Semigroups: VerifyIdentity: usage,\n",
                  "the second argument must be a non-empty list,");
  elif Size(id) <> 2 then
    ErrorNoReturn("Semigroups: VerifyIdentity: usage,\n",
                  "the second argument must be a list of size 2,");
  elif ForAny(id, x -> not IsHomogeneousList(x)) and not IsPosInt(id[1][1])
      and not IsPosInt(id[2][1]) then
    ErrorNoReturn("Semigroups: VerifyIdentity: usage,\n",
                  "the second argument must be a list of lists of ",
                  "positive integers,");
  fi;

  lhs := id[1];
  rhs := id[2];
  nr_letters := Maximum(Maximum(lhs), Maximum(rhs));

  for tup in IteratorOfTuples(coll, nr_letters) do
    if EvaluateWord(tup, lhs) <> EvaluateWord(tup, rhs) then
      return [tup];
    fi;
  od;
  return true;
end);

InstallMethod(NrLettersIdentity, "for a homogeneous list",
[IsHomogeneousList],
function(id)

  if IsEmpty(id) then
    ErrorNoReturn("Semigroups: NrLettersIdentity: usage,\n",
                  "the argument must be a non-empty list,");
  elif Size(id) <> 2 then
    ErrorNoReturn("Semigroups: NrLettersIdentity: usage,\n",
                  "the argument must be a list of size 2,");
  elif ForAny(id, x -> not IsHomogeneousList(x)) and not IsPosInt(id[1][1])
    and not IsPosInt(id[2][1]) then
    ErrorNoReturn("Semigroups: NrLettersIdentity: usage,\n",
                  "the argument must be a list of lists of positive integers,");
  fi;

  return Maximum(Maximum(id[1]), Maximum(id[2]));
end);

InstallMethod(ReverseIdentity, "for a homogeneous list",
[IsHomogeneousList],
function(id)

  if IsEmpty(id) then
    ErrorNoReturn("Semigroups: ReverseIdentity: usage,\n",
                  "the argument must be a non-empty list,");
  elif Size(id) <> 2 then
    ErrorNoReturn("Semigroups: ReverseIdentity: usage,\n",
                  "the argument must be a list of size 2,");
  elif ForAny(id, x -> not IsHomogeneousList(x)) and not IsPosInt(id[1][1])
    and not IsPosInt(id[2][1]) then
    ErrorNoReturn("Semigroups: ReverseIdentity: usage,\n",
                  "the argument must be a list of lists of positive integers,");
  fi;

  return List(id, Reversed);
end);

InstallMethod(RandomTable, "for a positive integer",
[IsPosInt],
function(n)
  return List([1 .. n], x -> List([1 .. n], y -> Random([1 .. n])));
end);

InstallMethod(RandomTuple, "for a positive integer",
[IsPosInt],
function(n)
  return List([1 .. n], x-> Random([0, 1]));
end);

InstallMethod(GroupAlgebraProduct,
"for a table a homogeneous list and a homogeneous list",
[IsTable, IsHomogeneousList, IsHomogeneousList],
function(table, x, y)
  local n, xy, r, s;

  if Length(x) <> Length(y) then
    ErrorNoReturn("Semigroups: GroupAlgebraProduct: usage,\n",
                  "the second and third arguments must be lists of the same ",
                  "size,");
  elif ForAny(x, w -> not w in [0, 1]) or ForAny(y, w -> not w in [0, 1]) then
    ErrorNoReturn("Semigroups: GroupAlgebraProduct: usage,\n",
                  "the second and third arguments should be lists whose ",
                  "entries are in [0, 1],");
  fi;

  n := Length(table);
  xy := [1 .. n] * 0;

  for r in [1 .. n] do
    for s in [1 .. n] do
      xy[table[r][s]] := (xy[table[r][s]] + x[r] * y[s]);
    od;
  od;

  return List(xy, val -> val mod 2);
end);

InstallMethod(RandomAssociativityTest, "for a table",
[IsTable],
function(table)
  local n, delta, nr, t, i;

  n := Length(table);
  delta := Float(Nr3NilpotentSemigroups(n, "Labelled") / n ^ (n ^ 2)) / 10^17;
  nr := Int(Ceil(Log2(1 / delta)));

  for i in [1 .. nr] do
    t := List([1 .. 3], x -> RandomTuple(n));
    if GroupAlgebraProduct(table, GroupAlgebraProduct(table, t[1], t[2]), t[3])
        <> GroupAlgebraProduct(table, t[1], GroupAlgebraProduct(table,
                                                                t[2], t[3]))
    then
      return false;
    fi;
  od;
  return true;
end);
