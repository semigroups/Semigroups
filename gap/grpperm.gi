#############################################################################
##
#W  grpperm.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# in this file there are some methods for perm groups that were not found in the
# library.

# returns an iterator of the sorted elements of the stab chain S^conj.

InstallGlobalFunction(IteratorSortedConjugateStabChain,
function(S, conj)
  local SortedStabChain, record, indices, T, iter;

  # finds the element of the group with stab chain S corresponding to the tuple
  # <indices>.
  SortedStabChain := function(S, rep, indices, level)
    local pnt, x, val, next, gen;

    if Length( S.generators ) = 0  then
      return rep;
    fi;

    pnt := S.orbit[1];
    x := conj * rep;
    next := AsSet(OnTuples(S.orbit, x))[indices[level]] / x;

    while next <> pnt do
      gen := S.transversal[next];
      rep := LeftQuotient( gen ^ conj, rep );
      next := next ^ gen;
    od;

    return SortedStabChain( S.stabilizer, rep, indices, level + 1);
  end;

  record := rec();

  # find the lengths of the orbits in the chain
  indices := [];
  T := S;

  while Length(T.generators) <> 0 do
    Add(indices, [ 1 .. Length(T.orbit) ]);
    T := T.stabilizer;
  od;

  record.indices := IteratorOfCartesianProduct(indices);
  record.stabchain := S;

  record.NextIterator := function(iter)
    if IsDoneIterator(iter!.indices) then
      return fail;
    fi;
    return SortedStabChain(iter!.stabchain, (), NextIterator(iter!.indices), 1);
  end;

  record.ShallowCopy := function(iter)
    return rec( indices := ShallowCopy(iter!.indices),
                stabchain := iter!.stabchain);
  end;

  iter := IteratorByNextIterator(record);
  SetFilterObj(iter, IsIteratorSorted);
  return iter;
end);

# finds the largest element of the stab chain S^conj.

InstallGlobalFunction(LargestElementConjugateStabChain,
function(S, rep, conj)
  local pnt, max, val, gen, i;

    if Length( S.generators ) = 0  then
      return rep;
    fi;

    pnt := S.orbit[1];
    max := 0;
    val := 0;

    for i in S.orbit  do
      if (i ^ conj) ^ rep > val  then
        max := i;
        val := (i ^ conj) ^ rep;
      fi;
    od;

    while pnt <> max  do
      gen := S.transversal[max];
      rep := LeftQuotient( gen ^ conj, rep );
      max := max ^ gen;
    od;

    return LargestElementConjugateStabChain( S.stabilizer, rep, conj );
end);

