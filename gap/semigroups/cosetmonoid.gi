#############################################################################
##
##  cosetmonoid.gi
##  Copyright (C) 2020                                  Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallMethod(CosetMonoid, "for a perm group", [IsPermGroup],
function(G)
  local genprod, cosets, subgroup, subgroups, gens, cosetpositions, i, j;

  subgroups := AllSubgroups(G);

  cosets := List(subgroups, x -> RightCosets(G, x));
  cosetpositions := [];

  for i in [1 .. Size(cosets)] do
    for j in [1 .. Size(cosets[i])] do
      Add(cosetpositions, [i, j]);
    od;
  od;
  Sort(cosetpositions);

  genprod := function(i, j, E)
    local newgroup, rep, newgens;
    if E in Generators(G) then
      return Position(cosetpositions,
                      [i, Position(cosets[i], cosets[i][j] * E)]);
    else
      rep := Representative(cosets[i][j]);
      newgens := Concatenation(Generators(subgroups[i]),
                  Generators(E ^ (rep ^ -1)));
      if newgens = [] then 
        newgens := [()];
      fi;
      newgroup := Group(newgens);
      newgroup := Position(subgroups, newgroup);
      rep := Position(cosets[newgroup], RightCoset(subgroups[newgroup], rep));
      return Position(cosetpositions, [newgroup, rep]);
    fi;
  end;

  gens := [];

  for subgroup in subgroups do
    if IsCyclic(subgroup) then
      Add(gens, subgroup);
    fi;
  od;

  gens := Concatenation(gens, Generators(G));

  Apply(gens, x -> 
        Transformation(List(cosetpositions, y -> genprod(y[1], y[2], x))));

  return Semigroup(gens);
end);
