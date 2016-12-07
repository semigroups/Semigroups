############################################################################
##
#W  elements.gi
#Y  Copyright (C) 2016                                   Wilfred A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################

SEMIGROUPS.IndexPeriodByRank := function(x, rank)
  local k, p, s, rank_s, powers, min_rank, index, lower, next, mid, y, z,
  period, S;

  k := rank(x);
  p := 1;
  s := x ^ 2;
  rank_s := rank(s);
  powers := [x, s]; # powers[i] = x ^ (2 ^ (i - 1)), e.g. powers[5] = x ^ 16.

  # Locate the index between its closest powers of 2
  while rank_s <= k - (2 ^ (p - 1)) do
    k := rank_s;
    p := p + 1;
    s := s ^ 2;
    rank_s := rank(s);
    Add(powers, s);
  od;

  min_rank := rank_s;

  if rank(x) = min_rank then
    index := 1;
  else
    # Get the specific values of the closest powers of 2 to index
    if k = min_rank then
      lower := p - 2;
    else
      lower := p - 1;
    fi;
    # (2 ^ lower) < index of x <= (2 ^ (lower + 1))

    index := (2 ^ lower) + 1; # index is always a lower bound for the true index
    next := lower;
    lower := powers[lower + 1];

    # Perform a 'binary search' to completion to nail down the position of index
    # The index is the *least* number such that x ^ index = min_rank
    while next > 0 do
      mid := lower * powers[next];
      next := next - 1;
      if rank(mid) <> min_rank then
        lower := mid;
        index := index + (2 ^ next);
      fi;
    od;
  fi;

  y := x ^ index;
  z := y * x;
  if y = z then
    period := 1;
  elif IsMultiplicativeElementWithOne(y) and One(y) <> fail and z = One(y) then
    period := 2;
  else
    S := Semigroup(y, z);
    SetIsGroupAsSemigroup(S, true);
    period := Size(S);
  fi;
  return [index, period];
end;

InstallMethod(IndexPeriodOfSemigroupElement, "for a multiplicative element",
[IsMultiplicativeElement],
function(x)
  local index, y, z, period, S;

  if not IsGeneratorsOfSemigroup([x]) then
    ErrorNoReturn("Semigroups: IndexPeriodOfSemigroupElement: usage,\n",
                  "the argument <x> must be the generator of a semigroup,");
  fi;
  index := NrDClasses(Semigroup(x));
  y := x ^ index;
  z := y * x;
  if y = z then
    period := 1;
  elif IsMultiplicativeElementWithOne(y) and One(y) <> fail and z = One(y) then
    period := 2;
  else
    S := Semigroup(y, z);
    SetIsGroupAsSemigroup(S, true);
    period := Size(S);
  fi;
  return [index, period];
end);

InstallMethod(SmallestIdempotentPower, "for a multiplicative element",
[IsMultiplicativeElement],
function(x)
  local a, ind, per, pow;

  if not IsGeneratorsOfSemigroup([x]) then
    ErrorNoReturn("Semigroups: SmallestIdempotentPower: usage,\n",
                  "the argument <x> must be the generator of a semigroup,");
  fi;
  a := IndexPeriodOfSemigroupElement(x);
  ind := a[1];
  per := a[2];
  pow := per;
  while pow < ind do
    pow := pow + per;
  od;
  return pow;
end);

SEMIGROUPS.RandomIDTest := function(S, llhs, lrhs)
  local varl, varr, idl, idr, tup, sztup, iter, i, nr, list, i2, bl, pos, npos,
  i3;
  idl := EmptyPlist(Size(llhs));
  idr := EmptyPlist(Size(lrhs));
  tup := EmptyPlist(1);
  if Maximum(llhs) < Maximum(lrhs) then
    sztup := Maximum(lrhs);
  else
    sztup := Maximum(llhs);
  fi;
  iter := IteratorOfTuples(S, sztup);
  for i in iter do
    tup := i;
    nr := 0;
    list := EmptyPlist(sztup);
    for i2 in llhs do
      bl := i2 in list;
      if bl = false then
        nr := nr + 1;
        list[nr] := i2;
        pos := Positions(llhs, i2);
        npos := Size(pos);
        for i3 in [1 .. npos] do
          idl[pos[i3]] := tup[i2];
        od;
      fi;
    od;
    list := EmptyPlist(sztup);
    nr := 0;
    for i2 in lrhs do
      bl := i2 in list;
      if bl = false then
        nr := nr + 1;
        list[nr] := i2;
        pos := Positions(lrhs, i2);
        npos := Size(pos);
        for i3 in [1 .. npos] do
          idr[pos[i3]] := tup[i2];
        od;
      fi;
    od;
    for i2 in [1 .. Size(idl) - 1] do
      idl[i2 + 1] := idl[i2] * idl[i2 + 1];
    od;
    for i2 in [1 .. Size(idr) - 1] do
      idr[i2 + 1] := idr[i2] * idr[i2 + 1];
    od;
    if idl[Size(idl)] <> idr[Size(idr)] then
      return false;
    fi;
  od;
  return true;
end;
