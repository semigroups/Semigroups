

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

#EOF



