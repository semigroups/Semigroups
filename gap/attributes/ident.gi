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

InstallMethod(RandomAssociativityTest, "for a table",
[IsTable],
function(table)
  local n, delta, nr, t, i, GAProduct, nr3NilAll;

  GAProduct := function(tab, x, y)
                 local n, xy, r, s;

                 n := Length(tab);
                 xy := [1 .. n] * 0;

                 for r in [1 .. n] do
                   for s in [1 .. n] do
                     xy[tab[r][s]] := (xy[tab[r][s]] + x[r] * y[s]);
                   od;
                 od;

                 return List(xy, val -> val mod 2);
               end;    

  nr3NilAll := function( n )
                 return Sum([2 .. Int(n + 1 / 2 - RootInt(n - 1))], k ->
                            Binomial(n, k) * k * Sum([0 .. k - 1], i -> (-1) ^
                            i * Binomial(k - 1, i) * (k - i) ^ ((n - k) ^ 2)));
               end;

  n := Length(table);
  delta := Float(nr3NilAll(n) / n ^ (n ^ 2)) / 10 ^ 17;
  nr := Int(Ceil(Log2(1 / delta)));

  for i in [1 .. nr] do
    t := List([1 .. 3], x -> List([1 .. n], x -> Random([0, 1])));
    if GAProduct(table, GAProduct(table, t[1], t[2]), t[3])
       <> GAProduct(table, t[1], GAProduct(table, t[2], t[3])) then
      return false;
    fi;
  od;
  return true;
end);
