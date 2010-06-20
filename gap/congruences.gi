#############################################################################
##
#W  congruences.gi
#Y  Copyright (C) 2006-2010                             James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## $Id$
##

# - add info messages
# - should really have IteratorOfSemilatticeCongruences at the core...
# - tweak the VHF method and integrate it into the function 
#   CongruencesSemilatticeAsPositionsNC

###############################################################################
# checks which partitions of [1..n] are congruence of the smallsemi s with n 
# elements

InstallMethod(CongruencesOfSemigroupAsPositions, "for a semigroup",
[IsSemigroup],
function(s)
local part, out, p;
Info(InfoWarning, 1, "this function performs an exhaustive search over all",
" partitions of the elements of <s>");
Info(InfoWarning, 1, "as such it might take a long long time to finish for", 
"semigroups with more than, say, 10 elements");

part:=PartitionsSet([1..Size(s)]);
out:=[];

for p in part do 
  if IsCongruenceOfSemigroupNC(s, p) then 
    #Add(out, List(p, x-> Elements(s){x}));
    Add(out, p);
  fi;
od;

return Set(List(out, Set));
end);

###############################################################################
#
InstallMethod(CongruencesOfSemigroup, "for a semigroup",
[IsSemigroup],
function(s)
return List(CongruencesOfSemigroupAsPositions(s), x-> Elements(s){x});
end);

###############################################################################
#

InstallMethod(CongruencesSemilattice, "for a semilattice", 
[IsSemilatticeAsSemigroup],
function(s) 
local t;
Info(InfoMonoidCongruences, 4, "CongruencesSemilattice");

if IsSemilatticeAsSemigroup(s) then
  t:=Semigroup(Elements(s));
  SetIsFinite(t, true);
  # the previous line should be removed if and when 
  # smallsemi is fixed...
  return List(CongruencesSemilatticeAsPositions(s), 
   x-> List(x, y-> Elements(s){y}));
fi;

Info(InfoWarning, 1, "Usage: semilattice as semigroup");
return fail;
end);

###############################################################################
#

InstallMethod(CongruencesSemilatticeAsPositions, "for a semilattice",
[IsSemilatticeAsSemigroup],
function(s) 
local elts, cayley, convex, restrict, i, lower, j, t, min, run;

Info(InfoMonoidCongruences, 4, "CongruencesSemilatticeAsPositions");

if not IsSemilatticeAsSemigroup(s) then 
  Info(InfoWarning, 1, "Usage: a semilattice as semigroup");
  return fail;
fi;

elts:=Elements(s);
if not Size(s)=Length(GeneratorsOfSemigroup(s)) then
  Info(InfoMonoidCongruences, 2, "taking all elements as generating set");
	t:=Semigroup(elts);
	SetIsFinite(t, true);
else
	t:=s;
fi;

cayley:=CayleyGraphSemigroup(t);

Info(InfoMonoidCongruences, 2, "computing convex subsemigroups...");
run:=Runtime();
convex:=ConvexSubsemigroupsAsPositionsNC(t); 

Info(InfoMonoidCongruences, 3, Length(convex), " convex subsemigroups in", 
 StringTime(Runtime()-run));

Info(InfoMonoidCongruences, 2, "computing restrictions...");
restrict:=[];
run:=Runtime();
min:=OneMinEltSemilatticeAsPositionNC(cayley, [1..Size(s)]);

for i in convex do 
	if not min in i then 
		lower:=Difference(Union(cayley{i}), i);
		Add(restrict, List(lower, x-> List(Set(elts{i}*elts[x]), 
		 x-> Position(elts, x))));
	else
		Add(restrict, []);
	fi;
od;

Info(InfoMonoidCongruences, 3, "done in", StringTime(Runtime()-run));

return CongruencesSemilatticeAsPositionsNC(elts, cayley, [1..Length(elts)], 
   convex, [1..Length(convex)], restrict, []);   
end);

###############################################################################
# the following is the JDM method...
#
# returns the congruences of the semigroup with elements <elts> and Cayley
# graph <cayley> (assuming that the cayley graph is w.r.t. the entire semigroup
# as generating set), that are contained in <elts{subset}> and have classes 
# belonging to <convex{convex_index}> subjected to the restrictions imposed
# by <restrict> and the existing classes of the congruence <current> on the 
# complement of <elts{subset}>.

InstallGlobalFunction(CongruencesSemilatticeAsPositionsNC,
function(elts, cayley, subset, convex, convex_index, restrict, current)
local cong_list, min, j, subset2, convex2, k, lower, l, ll, x, y,
 stop, current2;

#Info(InfoMonoidCongruences, 4, "CongruencesSemilatticeAsPositionsNC");

cong_list:=[]; 

min:=OneMinEltSemilatticeAsPositionNC(cayley, subset);

for j in convex_index do
  if min in convex[j] then
    subset2:=Difference(subset, convex[j]);
    convex2:=[];
    current2:=Concatenation(current, [convex[j]]);
    
    if not subset2=[] then 
			for k in Filtered(convex_index, x-> IsSubset(subset2, convex[x])) do 
				if Length(restrict[k])>0 then 
				#filtering for this earlier slow things down
					l:=0; 
					stop:=false;
			 
					repeat
						l:=l+1;
						x:=restrict[k][l];
						ll:=0;
						if not Length(restrict[k][l])=1 then 
						#filtering for this earlier slow things down
							repeat
								ll:=ll+1;
								y:=current2[ll];
								if not Intersection(x,y)=[] and not IsSubset(y, x) then 
									stop:=true;
								fi;
							until stop or ll=Length(current2);
						fi;
					until stop or l=Length(restrict[k]);
	
					if not stop then 
						Add(convex2, k);
					fi;
				else
					convex2:=Filtered(convex_index, x-> IsSubset(subset2, convex[x]));
				fi;
			od;
      Append(cong_list, List(CongruencesSemilatticeAsPositionsNC(elts, 
       cayley, subset2, convex, convex2, restrict, current2), 
        x-> Concatenation(x, [convex[j]])));
    else
      Add(cong_list, [convex[j]]);
    fi;
  fi;
od;

#return cong_list;
return Set(List(cong_list, Set));
end);

###############################################################################

InstallGlobalFunction(ConvexSubsemigroup,
function(s, t)
local u;

Info(InfoMonoidCongruences, 4, "ConvexSubsemigroup");

if IsSemilatticeAsSemigroup(s) and IsSubset(Elements(s), t) then 
  if Length(GeneratorsOfSemigroup(s))=Size(s) then 
    u:=s;
  else
    u:=Semigroup(Elements(s));
    SetIsFinite(u, true);
  fi;
  return ConvexSubsemigroupNC(u, t);
fi;

Info(InfoWarning, 1, "Usage: semilattice and subset of its elements");
return fail;
end);

###############################################################################
#the convex subsemigroup generated by the elements t in s

InstallGlobalFunction(ConvexSubsemigroupNC,
function(s, t)
local subset, out;

Info(InfoMonoidCongruences, 4, "ConvexSubsemigroupNC");

subset:=List(t, x-> Position(Elements(s), x));
out:=ConvexSubsemigroupAsPositionsNC(CayleyGraphSemigroup(s), subset);
return List(out, x-> Elements(s)[x]);
end);

###############################################################################
# the following assumes cayley is a cayley graph of a semilattice generated
# by all of its elements and subset is a set of pos. ints.

InstallGlobalFunction(ConvexSubsemigroupAsPositionsNC,
function(cayley, subset)
local out, x, y, z;
out:=subset;

for x in subset do 
  for y in [1..Size(cayley)] do 
    for z in subset do
      if y in cayley[x] and z in cayley[y] then 
        AddSet(out, y);
      fi;
    od;
  od;
od;

return out;
end);

###############################################################################
# return all convex subsemigroups of s

InstallMethod(ConvexSubsemigroups, "for a semilattice",
[IsSemilatticeAsSemigroup], 
function(s)
local t, convex;

Info(InfoMonoidCongruences, 4, "ConvexSubsemigroups");

if Length(GeneratorsOfSemigroup(s))=Size(s) then 
  t:=s;
else
  t:=Semigroup(GeneratorsOfSemigroup(s));
  SetIsFinite(t, true);
fi;

convex:=ConvexSubsemigroupsAsPositions(t);
return List(convex, x-> Elements(t){x});
end);

###############################################################################

InstallMethod(ConvexSubsemigroupsAsPositions, "for a semilattice",
[IsSemilatticeAsSemigroup],
function(s)
local t;

Info(InfoMonoidCongruences, 4, "ConvexSubsemigroupsAsPositions");

if Length(GeneratorsOfSemigroup(s))=Size(s) then 
  t:=s;
else
  t:=Semigroup(GeneratorsOfSemigroup(s));
  SetIsFinite(t, true);
fi;

return ConvexSubsemigroupsAsPositionsNC(t);
end);

###############################################################################
# return all convex subsemigroups of semilattice.

InstallGlobalFunction(ConvexSubsemigroupsAsPositionsNC,
function(arg)
local anti, out, i, j, k, anti2, elts, cayley, t;

Info(InfoMonoidCongruences, 4, "ConvexSubsemigroupsAsPositionsNC");

if Length(arg)=1 then 
  elts:=Elements(arg[1]);
  cayley:=CayleyGraphSemigroup(arg[1]); #assume arg[1] is generated by all elts
elif Length(arg)=2 then 
  elts:=arg[1];
  cayley:=arg[2];
else 
  return fail;
fi;

#get antichains

Info(InfoMonoidCongruences, 2, "finding maximal antichains...");
t:=Runtime();
anti:=CompleteSubgraphs(Graph(Group(()), [1..Length(cayley)], OnPoints, 
 function(i,j) return not i in cayley[j] and not j in cayley[i]; end, true));
anti2:=[];
out:=[];

Info(InfoMonoidCongruences, 3, Length(anti), " maximal antichains in", StringTime(Runtime()-t));

#JDM maybe use a hash table here...

Info(InfoMonoidCongruences, 2, "finding all antichains...");

t:=Runtime();
for i in anti do 
  for j in Combinations(i) do 
    if not j=[] then 
      AddSet(anti2, j);
    fi;
  od;
od; #JDM better not do this use something like IteratorOfCombinationsNC in the loop below...

Info(InfoMonoidCongruences, 3, Length(anti2), " antichains in", StringTime(Runtime()-t));

Info(InfoMonoidCongruences, 2, "finding convex subsemigroups");

t:=Runtime();
for i in anti2 do
  for k in Set(cayley[Position(elts, Product(elts{i}))]) do 
    AddSet(out, ConvexSubsemigroupAsPositionsNC(cayley, Union(i, [k])));
  od;
od;

Info(InfoMonoidCongruences, 3, Length(out), " convex subsemigroups in", StringTime(Runtime()-t));

return out;
end);

###############################################################################
#

InstallGlobalFunction(IsCongruenceOfSemigroupNC, "for a semigroup and a partition of its elements",
function(s, p)
local i, j, k, t;

Info(InfoMonoidCongruences, 4, "IsCongruenceOfSemigroupNC");

if HasGeneratorsOfSemigroup(s) then 
  t:=GeneratorsOfSemigroup(s);
else 
  t:=s;
fi;

for i in p do 
  for j in t do 
    k:=i*j;
    if not ForAny(p, x-> IsSubset(x, k)) then 
      return false;
    fi;
    if not IsCommutative(s) then 
      k:=j*i;
      if not ForAny(p, x-> IsSubset(x, k)) then 
        return false;
      fi;
    fi;
  od;
od;

return true;
end);

###############################################################################
#

InstallGlobalFunction(IsCongruenceOfSemigroup, "for a semigroup and a partition of its elements",
function(s, p)

Info(InfoMonoidCongruences, 4, "IsCongruenceOfSemigroup");

if IsSemilatticeAsSemigroup(s) and IsList(p) and ForAll(p, IsList) and 
 Union(p)=Elements(s) and  ForAll(p, x-> ForAll(x, y-> y in s)) and 
  Sum(List(p, Length))=Size(s) then 
   return IsCongruenceOfSemigroupNC(s, p);
fi;

Info(InfoWarning, 1, "Usage: semigroup, and partition of its elements");
return fail;
end);


###############################################################################
# a subsemigroup T of a semilattice S is convex if for all x,y in T and for all 
# z in S if x<z<y then z in T. 

InstallGlobalFunction(IsConvexSubsetOfSemilatticeNC, 
function(s, t)
local g, x, y, z, pos;

Info(InfoMonoidCongruences, 4, "IsConvexSubsetOfSemilatticeNC");

g:=CayleyGraphSemigroup(s); 
#this is transitive if s is generated by all its elements!
pos:=Filtered([1..Size(s)], x-> Elements(s)[x] in t);

for x in pos do 
  for y in [1..Size(s)] do 
    for z in pos do
      if y in g[x] and z in g[y] and not Elements(s)[y] in t then 
        return false;
      fi;
    od;
  od;
od;

return true;
end);

###############################################################################

InstallGlobalFunction(IsConvexSubsetOfSemilattice, 
function(s, t)
local u;

Info(InfoMonoidCongruences, 4, "IsConvexSubsemigpOfSemilattice");

if IsSemilatticeAsSemigroup(s) and ForAll(t, x-> x in s) then 
  if Length(GeneratorsOfSemigroup(s))=Size(s) then 
    u:=s;
  else
    u:=Semigroup(GeneratorsOfSemigroup(s));
    SetIsFinite(u, true);
  fi;
  return IsConvexSubsetOfSemilatticeNC(u, t);
fi;

Info(InfoWarning, 1, "Usage: semilattice and a subset");
return fail;
end);


###############################################################################
#

InstallGlobalFunction(OneMinEltSemilatticeAsPositionNC, 
function(cayley, subset)

#Info(InfoMonoidCongruences, 4, "OneMinEltSemilatticeAsPositionNC");

return First(subset, x-> ForAll(cayley[x], y-> not y in subset or x=y));
end);

###############################################################################
#

InstallGlobalFunction(OneMinEltSemilatticeAsPosition, 
function(arg)
local t, subset;
#Info(InfoMonoidCongruences, 4, "OneMinEltSemilatticeAsPosition");

if IsSemilatticeAsSemigroup(arg[1]) then 
  if Size(arg[1])=Length(GeneratorsOfSemigroup(arg[1])) then 
		t:=arg[1];
	else
		t:=Semigroup(Elements(arg[1]));
		SetIsFinite(t, true); #remove when smallsemi is fixed!
	fi;

  if Length(arg)=2 and IsCyclotomicCollection(arg[2]) and 
   ForAll(arg[2], IsPosInt) and ForAll(arg[2], x-> x<=Size(t)) then 
    subset:=arg[2];
  else 
    subset:=[1..Size(arg[1])];
  fi;
  return OneMinEltSemilatticeAsPositionNC(CayleyGraphSemigroup(t), subset);
fi;   

Info(InfoWarning, 1, "Usage: semilattice <s>[, subset of <[1..Size(s)]>]");
return fail;
end);

###############################################################################
#

InstallGlobalFunction(OneMinEltSemilattice, 
function(arg)
local t, subset;
#Info(InfoMonoidCongruences, 4, "OneMinEltSemilatticeAsPosition");

if IsSemilatticeAsSemigroup(arg[1]) then 
  if Size(arg[1])=Length(GeneratorsOfSemigroup(arg[1])) then 
		t:=arg[1];
	else
		t:=Semigroup(Elements(arg[1]));
		SetIsFinite(t, true); #remove when smallsemi is fixed!
	fi;

  if Length(arg)=2 and IsList(arg[2]) and ForAll(arg[2], x-> x in arg[1]) then 
    subset:=List(arg[2], x-> Position(Elements(arg[1]), x));
  else 
    subset:=[1..Size(arg[1])];
  fi;
  return Elements(arg[1])
   [OneMinEltSemilatticeAsPositionNC(CayleyGraphSemigroup(t), subset)];
fi;   

Info(InfoWarning, 1, "Usage: semilattice <s>[, subset of <[1..Size(s)]>]");
return fail;
end);

###############################################################################
#

# END!

MaximalEltsSemilattice:=function(cayley, subset)
local j, out;

out:=[];
for j in subset do
  if ForAll(subset, k-> j=k or not j in cayley[k]) then 
    Add(out, j);
  fi;
od;
#Sort(out, function(x,y) return Length(Set(cayley[x]))>Length(Set(cayley[y])); end );
return out;
end;

###############################################################################
# the following is the VHF method

CongruencesSemilatticeNC2:=function(elts, cayley, subset, cong_list)
local min, max, cong2, cong, i, j, k, l, stop, cong_list2, min2;

min:=OneMinEltSemilatticeAsPositionNC(cayley, subset);
max:=MaximalEltsSemilattice(cayley, Difference(Set(cayley[min]), [min]));
cong_list2:=[];
SubtractSet(subset, [min]);
if not cong_list=[] then 
	for cong in cong_list do
		Add(cong_list2, Concatenation(cong, [[min]]));
		for i in [1..Length(cong)] do
		  if ForAny(max, x-> x in cong[i]) then  
				#check condition for
				j:=Union(cong[i], [min]);
				l:=0;
				stop:=false;
				#if Length(cong)>1 then 
					repeat
						l:=l+1;
						if not l=i and ForAny(max, x-> x in cong[l]) then 
							min2:=OneMinEltSemilatticeAsPositionNC(cayley, cong[l]);
							if ForAny(MaximalEltsSemilattice(cayley, j), x-> 
								min2 in cayley[x]) then 
								stop:=not min2 in 
								 cayley[OneMinEltSemilatticeAsPositionNC(cayley, j)];
							fi;
						fi;
					until l=Length(cong) or stop;
				#fi;
				
				if not stop then 
					cong2:=ShallowCopy(cong);
					cong2[i]:=j;
					Add(cong_list2, cong2);
				fi;
			fi;
		od;
	od;
else
  cong_list2:=[[[min]]];
fi;

if not subset=[] then 
  return CongruencesSemilatticeNC2(elts, cayley, subset, cong_list2);
fi;

return cong_list2;
end;

###############################################################################
#

CongruencesSemilattice2:=function(s)
return CongruencesSemilatticeNC2(Elements(s), 
 CayleyGraphSemigroup(Semigroup(Elements(s))), [1..Size(s)], []);
end;