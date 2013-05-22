

InstallOtherMethod(HClassReps, "for an D-class of inv semi of partial perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local scc, mults, f, out, k, i, j;

  scc:=OrbSCC(d!.o)[d!.data[1]]; 
  mults:=OrbMultipliers(d);
  f:=Representative(d);
  out:=EmptyPlist(Length(scc)^2); 

  k:=0;
  for i in scc do 
    for j in scc do 
      k:=k+1;
      out[k]:=mults[i]*f/mults[j];
    od;
  od;
  return out;
end);


InstallOtherMethod(HClassReps, "for an L-class of inv semi of partial perms",
[IsGreensLClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(l)
  local scc, mults, f, out, j, i;

  scc:=OrbSCC(l!.o)[l!.data[1]]; 
  mults:=OrbMultipliers(l);
  f:=Representative(l);
  out:=EmptyPlist(Length(scc)); 

  j:=0;
  for i in scc do 
    j:=j+1;
    out[j]:=mults[i]*f;
  od;
  return out;
end);


InstallOtherMethod(HClassReps, "for an R-class of inv semi of partial perms",
[IsGreensRClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(r)
  local scc, mults, f, out, j, i;

  scc:=OrbSCC(r!.o)[r!.data[1]]; 
  mults:=OrbMultipliers(r);
  f:=Representative(r);
  out:=EmptyPlist(Length(scc)); 

  j:=0;
  for i in scc do 
    j:=j+1;
    out[j]:=f/mults[i];
  od;
  return out;
end);

InstallMethod(Iterator, "for D-class of inv semigroup",
[IsGreensDClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> Representative(d)*x),

      at:=[0,1,1],

      m:=Length(OrbSCC(d!.o)[d!.data[1]]), n:=Size(SchutzenbergerGroup(d)),

      IsDoneIterator:=iter-> 
       iter!.at[1]=iter!.m and iter!.at[2]=iter!.n and iter!.at[3]=iter!.m,

      NextIterator:=function(iter)
        local at, scc;

        at:=iter!.at;
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if at[1]<iter!.m then 
          at[1]:=at[1]+1;
        elif at[3]<iter!.m then 
          at[1]:=1; at[3]:=at[3]+1;
        elif at[2]<iter!.n then 
          at[1]:=1; at[3]:=1; at[2]:=at[2]+1;
        fi;

        scc:=OrbSCC(d!.o)[d!.data[1]];
        return OrbMultipliers(d)[scc[at[1]]]*iter!.schutz[at[2]]/
         OrbMultipliers(d)[scc[at[3]]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1,1])));
  fi;

  SetIsIteratorOfDClassElements(iter, true);
  
  return iter;
end);

InstallMethod(Iterator, "for H-class of inv semigroup",
[IsGreensHClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(h)
  local iter;
  if HasAsSSortedList(h) then 
    iter:=IteratorList(AsSSortedList(h));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=Enumerator(SchutzenbergerGroup(h)),
      
      pre:=Representative(h)*OrbMultipliers(h)[h!.data[3]],
  
      post:=OrbMultipliers(h)[h!.data[3]]^-1,

      at:=0,

      IsDoneIterator:=iter-> iter!.at=Length(iter!.schutz),
       
      NextIterator:=function(iter)

        if IsDoneIterator(iter) then 
          return fail;
        fi;

        iter!.at:=iter!.at+1;

        return iter!.pre*iter!.schutz[iter!.at]*iter!.post;
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, pre:=iter!.pre, 
       post:=iter!.post, at:=0)));
  fi;

  SetIsIteratorOfHClassElements(iter, true);
  return iter;
end);

InstallMethod(Iterator, "for L-class of inv semigroup",
[IsGreensLClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> x*Representative(d)),

      at:=[0,1],

      m:=Length(OrbSCC(d!.o)[d!.data[1]]), n:=Size(SchutzenbergerGroup(d)),

      IsDoneIterator:=iter-> 
       iter!.at[1]=iter!.m and iter!.at[2]=iter!.n,

      NextIterator:=function(iter)
        local at;

        at:=iter!.at;
        if IsDoneIterator(iter) then 
          return fail;
        fi;

        if at[1]<iter!.m then 
          at[1]:=at[1]+1;
        else
          at[1]:=1; at[2]:=at[2]+1;
        fi;

        return OrbMultipliers(d)[OrbSCC(d!.o)[d!.data[1]][at[1]]]
         *iter!.schutz[at[2]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1])));
  fi;

  SetIsIteratorOfLClassElements(iter, true);
  return iter;
end);

# JDM this should have a method like IteratorOfRClassData and
# IteratorOfLClassData since the scc:=OrbSCC line makes this work not so
# well!

InstallMethod(IteratorOfHClassData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  o:=LongOrb(s); scc:=OrbSCC(o);
  
  return IteratorByFunctions( rec(
                 
    o:=o, m:=offset+1, at:=[1,0], scc_limit:=Length(scc),
    at_limit:=[Length(scc[Length(scc)]), Length(scc[Length(scc)])],

    IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
     iter!.at=iter!.at_limit,

    NextIterator:=function(iter)
      local o, scc, at, m, mults;

      m:=iter!.m; at:=iter!.at;

      if m=iter!.scc_limit and at=iter!.at_limit then 
        return fail;
      fi;
     
      o:=iter!.o; scc:=OrbSCC(o); 
      if at[2]<Length(scc[m]) then 
        at[2]:=at[2]+1;
      elif at[1]<Length(scc[m]) then 
        at[1]:=at[1]+1;
        at[2]:=1;
      else
        at:=[1,1];
        m:=m+1;
      fi;

      iter!.at:=at; iter!.m:=m;

      mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
      
      return [s, [m, scc[m][at[1]], scc[m][at[2]]], LongOrb(s),
       mults[scc[m][at[1]]]*PartialPermNC(o[scc[m][1]],
       o[scc[m][1]])*mults[scc[m][at[2]]]^-1];
    end,

    ShallowCopy:=iter-> rec(o:=iter!.o, m:=iter!.m, at:=[0,1],
    scc_limit:=iter!.scc_limit, at_limit:=iter!.at_limit)));
end);

InstallMethod(IteratorOfHClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasHClassReps(s) then 
    return IteratorList(HClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfHClassData(s), x-> x[4],
   [IsIteratorOfHClassReps]);
end);


InstallMethod(IteratorOfHClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasGreensHClasses(s) then 
    return IteratorList(GreensHClasses(s));
  fi;
  return IteratorByIterator(IteratorOfHClassData(s), x->
   CallFuncList(CreateHClass, x), [IsIteratorOfHClasses]);
end);

InstallMethod(LClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateLClass(h!.parent, [data[1], scc[data[1]][1], data[3]], h!.o, 
   mults[data[2]]^-1*Representative(h));
end);


InstallOtherMethod(LClassReps, "for an inv semi of part perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, scc, out, gens, i, l, mults, f, j, k;

  o:=LongOrb(s);
  scc:=OrbSCC(o);
  out:=EmptyPlist(Length(o));
  gens:=o!.gens;

  if IsPartialPermMonoid(s) then
    i:=0;
  else
    i:=1;
  fi;

  l:=0;
  for j in [1+i..Length(scc)] do
    mults:=CreateOrbSCCMultipliers(gens, o, j, scc[j]);
    f:=PartialPermNC(o[scc[j][1]], o[scc[j][1]]);
    for k in scc[j] do
      l:=l+1;
      out[l]:=f/mults[k];
    od;
  od;
  return out;
end);

InstallOtherMethod(LClassReps, "for D-class of part perm inv semi",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  local scc, out, rep, mults, i, j;

  scc:=OrbSCC(d!.o)[d!.data[1]];
  out:=EmptyPlist(Length(scc));
  rep:=Representative(d);
  mults:=OrbMultipliers(d);

  i:=0;
  for j in scc do 
    i:=i+1;
    out[i]:=rep/mults[j];
  od;
  return out;
end);


InstallOtherMethod(LClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);


InstallMethod(RClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateRClass(h!.parent, [data[1], data[2], scc[data[1]][1]], h!.o, 
   Representative(h)*mults[data[3]]);
end);

InstallMethod(Size, "for an D-class of an inverse semigroup",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  return Length(OrbSCC(d!.o)[d!.data[1]])^2*Size(SchutzenbergerGroup(d));
end);

InstallMethod(Size, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  return Size(SchutzenbergerGroup(h));
end);

InstallMethod(Size, "for an L-class of an inverse semigroup",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(l)
  return Length(OrbSCC(l!.o)[l!.data[1]])*Size(SchutzenbergerGroup(l));
end);

InstallMethod(Size, "for an R-class of an inverse semigroup",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(r)
  return Length(OrbSCC(r!.o)[r!.data[1]])*Size(SchutzenbergerGroup(r));
end);

InstallMethod(Size, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  EnumerateInverseSemiData(s);
  return Size(s);
end); 

InstallOtherMethod(StructureDescription, "for group H-class of inv semi",
[IsGroupHClass and IsGreensClassOfPartPermSemigroup], 
h-> StructureDescription(Range(IsomorphismPermGroup(h))));

#EOF



