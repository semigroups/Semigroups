#############################################################################
##
#W  inverse.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#EEE

# new for 0.7! - Enumerator - "for D-class of part perm inverse semigroup"
##############################################################################

InstallMethod(Enumerator, "for D-class of part perm inv semigroup",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)

  return EnumeratorByFunctions(d, rec(

    schutz:=Enumerator(SchutzenbergerGroup(d)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local scc, n, m, r, q, q2, mults;
      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return enum!.schutz[pos]*Representative(d);
      fi;

      scc:=OrbSCC(d!.o)[d!.data[1]];
      n:=pos-1; m:=Length(enum!.schutz); r:=Length(scc);
      q:=QuoInt(n, m); q2:=QuoInt(q, r);
      pos:=[ n-q*m, q2, q  - q2 * r ]+1;
      mults:=OrbMultipliers(d);
      return mults[scc[pos[2]]]*enum[pos[1]]/mults[scc[pos[3]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, m, k, l, j, scc;
      
      rep:=Representative(d);
      
      if f[2]<>rep[2] then 
        Info(InfoCitrus, 1, "rank not equal to those of",
          " any of the D-class elements,");
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=d!.o; m:=d!.data[1];
      
      k:=Position(o, DomPP(f)); 
      if k=fail or not OrbSCCTruthTable(o)[m][k] then 
        Info(InfoCitrus, 1, "domain not equal to that of any",
        " D-class element,");
        return fail;
      fi;

      l:=Position(o, RanSetPP(f));
      if l=fail or not OrbSCCTruthTable(o)[m][l] then
        Info(InfoCitrus, 1, "range not equal to that of any",
        " D-class element,");
        return fail;
      fi;

      m:=o!.mults[k]^-1*f*o!.mults[l]; #LQuoPP
      j:=Position(enum!.schutz, MappingPermListList(DomPP(m), RanPP(m)));

      if j=fail then 
        return fail;
      fi;
      scc:=OrbSCC(d!.o)[d!.data[1]];

      return Length(enum!.schutz)*((Position(scc, k)-1)*Length(scc)
      +(Position(scc, l)-1))+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in d;
    end,

    Length:=enum-> Size(d),

    PrintObj:=function(enum)
      Print("<enumerator of D-class>");
      return;
    end));
end);

#GGG

# new for 0.7! - GreensHClasses - for an D-class of inv semi of partial perms
############################################################################

InstallOtherMethod(GreensHClasses, "for D-class of inv semi of partial perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, k, mults, f, i, j;
  
  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(o)[m]; s:=d!.parent;
  out:=EmptyPlist(Length(scc)^2);
  
  if HasHClassReps(d) then 
    reps:=HClassReps(d);
    k:=0;
    for i in scc do
      for j in scc do
        k:=k+1;
        out[k]:=CreateHClass(s, [m,i,j], o, reps[k]);
      od;
    od; 
  else
    reps:=EmptyPlist(Length(scc)^2);
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    
    k:=0; 
    for i in scc do
      for j in scc do
        k:=k+1;
        reps[k]:=mults[i]*f/mults[j];
        out[k]:=CreateHClass(s, [m,i,j], o, reps[k]);
      od;
    od;
    SetHClassReps(d, reps); 
  fi;

  return out;
end);

# new for 0.7! - GroupHClass - for a D-class of inv semi
############################################################################

InstallOtherMethod(GroupHClass, "for a D-class of inverse semi",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local h;

  h:=GreensHClassOfElementNC(d, Representative(d));
  SetIsGroupHClass(h, true);
  return h;
end);

# new for 0.7! - GreensLClasses - for a D-class of inv semi of part perms
##############################################################################

InstallOtherMethod(GreensLClasses, "for D-class of inv semi of part perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, mults, f, i;

  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(d!.o)[m]; s:=d!.parent; 
  out:=EmptyPlist(Length(scc));

  if HasLClassReps(d) then 
    reps:=LClassReps(d);
    for i in [1..Length(scc)] do 
      out[i]:=CreateLClass(s, [m,scc[1],scc[i]], o, reps[i]);
    od;
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    for i in [1..Length(scc)] do 
      reps[i]:=f/mults[scc[i]];
      out[i]:=CreateLClass(s, [m,scc[1],scc[i]], o, reps[i]);
    od;
    SetLClassReps(d, reps);
  fi;
  return out;
end);

# new for 0.7! - GreensRClasses - for a D-class of inv semi of part perms
##############################################################################

InstallOtherMethod(GreensRClasses, "for D-class of inv semi of part perms",
[IsGreensDClass and IsGreensClassOfInverseSemigroup and
IsGreensClassOfPartPermSemigroup],
function(d)
  local m, o, scc, s, out, reps, mults, f, i;

  m:=d!.data[1]; o:=d!.o; scc:=OrbSCC(o)[m]; s:=d!.parent; 
  
  out:=EmptyPlist(Length(scc));

  if HasRClassReps(d) then 
    reps:=RClassReps(d);
    for i in [1..Length(scc)] do 
      out[i]:=CreateRClass(s, [m,scc[i],scc[1]], o, reps[i]);
    od;
  else
    reps:=EmptyPlist(Length(scc));
    mults:=OrbMultipliers(d);
    f:=Representative(d);
    for i in [1..Length(scc)] do 
      reps[i]:=mults[scc[i]]*f;
      out[i]:=CreateRClass(s, [m,scc[i],scc[1]], o, reps[i]);
    od;
    SetRClassReps(d, reps);
  fi;
  return out;
end);

# new for 0.7! - GreensRClassOfElementNC - for D-class and part perm
##############################################################################
# Notes: data is: [scc index, scc[1], pos of dom, pos of ran]

InstallOtherMethod(GreensRClassOfElementNC, "for a D-class and part perm",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup, IsPartialPerm],
function(d, f)
  local o, m, l, rep, k;

  o:=d!.o; m:=d!.data[1];
  l:=Position(o, RanSetPP(f));
  
  if l=fail then 
    return fail;
  fi;

  CreateOrbSCCSchutzGp(o, m); #JDM replace this with something more sensible
  rep:=f*o!.mults[l];
  k:=Position(o, DomPP(f));

  if k=fail or not OrbSCCTruthTable(o)[m][k] then 
    return fail;
  fi;

  return CreateRClass(d!.parent, [m,k,o!.scc[m][1]], o, rep);
end);

#HHH

# new for 0.7! - HClassReps - for an D-class of inv semi of partial perms
############################################################################

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

# new for 0.7! - HClassReps - for an L-class of inv semi of partial perms
############################################################################

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

# new for 0.7! - HClassReps - for an R-class of inv semi of partial perms
############################################################################

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

# new for 0.7! - HClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(HClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensHClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#III

# new for 0.7! - Idempotents - "for a class of a part perm inv semigroup"
##############################################################################

InstallOtherMethod(Idempotents, "for a class of a part perm inv semigroup",
[IsGreensClass and IsGreensClassOfInverseSemigroup and IsGreensClassOfPartPermSemigroup],
function(class)

  if IsGreensRClass(class) then 
    return [LeftOne(Representative(class))];
  elif IsGreensLClass(class) then 
    return [RightOne(Representative(class))];
  elif IsGreensDClass(class) then 
    return List(OrbSCC(class!.o)[class!.data[1]], x-> 
     PartialPermNC(class!.o[x],class!.o[x]));
  elif IsGreensHClass(class) and IsGroupHClass(class) then 
    return [LeftOne(Representative(class))];
  fi;
  return [];
end);

# new for 0.7! - Idempotents - "for a part perm inv semigroup"
##############################################################################

InstallOtherMethod(Idempotents, "for a part perm inv semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local o, r, l, out, i;
  
  o:=LongOrb(s);
  if not IsClosed(o) then 
    Enumerate(o, infinity);
  fi;
  r:=Length(o);
  
  if IsPartialPermMonoid(s) then 
    l:=0;
  else
    l:=1;
  fi;

 out:=EmptyPlist(r-l);
 
  for i in [1..r-l] do
    out[i]:=PartialPermNC(o[i+l], o[i+l]);
  od;
  return out;
end);

# new for 0.7! - Idempotents - "for a part perm inv semigroup and pos int"
##############################################################################

InstallOtherMethod(Idempotents, "for a part perm inv semigroup and pos int",
[IsInverseSemigroup and IsPartialPermSemigroup, IsInt],
function(s, i)
  local o, j, out, k;
  
  if i<0 then 
    Error("the argument should be a non-negative integer,");
  fi;

  if i>MaximumList(List(Generators(s), Rank)) then 
    return [];
  fi;

  o:=LongOrb(s); 
  k:=0;

  if IsClosed(o) then 
    out:=EmptyPlist(Length(o));
    for j in o do 
      if Length(j)=i then 
        k:=k+1;
        out[k]:=PartialPermNC(j, j);
      fi;
    od;
    ShrinkAllocationPlist(out);
    return out;
  fi;

  o:=Orb(s, Points(s), OnIntegerSetsWithPP,
        rec(forflatplainlists:=true, hashlen:=CitrusOptionsRec.hashlen.M,
        gradingfunc:=function(o, x) return Length(x); end,
        onlygrades:=function(x, y) return x>=i; end));

  Enumerate(o, infinity);
  out:=EmptyPlist(Length(o));
  for j in [1..Length(o)] do 
    if Grades(o)[j]=i then 
      k:=k+1;
      out[k]:=PartialPermNC(o[j], o[j]);
    fi;
  od;
  ShrinkAllocationPlist(out);
  return out;
end);

# new for 0.7! - IsGroupHClass - "for a part perm inv semigroup"
##############################################################################

InstallOtherMethod(IsGroupHClass, "for H-class of part perm inv semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and 
IsGreensClassOfInverseSemigroup], h-> 
 DomPP(Representative(h))=RanSetPP(Representative(h)));

# new for 0.9999! - IsJoinIrreducible - "for an inv semi and partial perm"
##############################################################################
# an element x of an inverse semigroup S is *join irreducible*, if 
# x=sup(U) for some subset U of S implies x in U (where sup is taken wrt the
# natural partial order. 

InstallGlobalFunction(IsJoinIrreducible,
function(S, x)
  local y, elts, i, k, j;

  if x=MultiplicativeZero(S) then 
    return false;
  elif x in MinimalIdeal(S) then 
    return true;
  fi;

  y:=Representative(GreensDClassOfElement(S, x));
  elts:=Set(Idempotents(S));
  i:=Position(elts, y);

  k:=0;

  for j in [i-1,i-2 ..1] do
    if NaturalLeqPP(elts[j], elts[i]) then
      k:=j;
      break;
    fi;
  od;

  for j in [k-1,k-2..1] do 
    if NaturalLeqPP(elts[j], elts[i]) and not NaturalLeqPP(elts[j], elts[k])
     then 
      return false;
    fi;
  od;
  return true;
end);  

# new for 0.7! - IsRegularDClass - "for D-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularDClass, IsGreensDClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.7! - IsRegularLClass - "for L-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularLClass, IsGreensLClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.7! - IsRegularRClass - "for R-class of inv semigroup"
##############################################################################

InstallTrueMethod(IsRegularRClass, IsGreensRClass and 
IsGreensClassOfInverseSemigroup); 

# new for 0.1! - Iterator - "for an inverse semigroup"
#############################################################################

InstallMethod(Iterator, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup], 
function(s)
  local iter;

  iter:= IteratorByFunctions( rec(

    R:=IteratorOfDClasses(s),

    r:=fail, s:=s,

    NextIterator:=function(iter)

      if IsDoneIterator(iter!.R) and IsDoneIterator(iter!.r) then
        return fail;
      fi;

      if iter!.r=fail or IsDoneIterator(iter!.r) then
        iter!.r:=Iterator(NextIterator(iter!.R));
      fi;

      return NextIterator(iter!.r);
    end,

    IsDoneIterator:= iter -> IsDoneIterator(iter!.R) and
     IsDoneIterator(iter!.r),

    ShallowCopy:= iter -> rec(R:=IteratorOfDClasses(s), r:=fail)));

  SetIsIteratorOfSemigroup(iter, true);

  return iter;
end);

# new for 0.7! - Iterator - "for D-class of inv semigroup"
##############################################################################

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

# new for 0.7! - Iterator - "for H-class of inv semigroup"
##############################################################################

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

# new for 0.7! - Iterator - "for L-class of inv semigroup"
##############################################################################

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

# new for 0.7! - Iterator - "for R-class of inv semigroup"
##############################################################################

InstallMethod(Iterator, "for R-class of inv semigroup",
[IsGreensRClass and IsGreensClassOfInverseSemigroup
and IsGreensClassOfPartPermSemigroup],
function(d)
  local iter;
  if HasAsSSortedList(d) then 
    iter:=IteratorList(AsSSortedList(d));
  else
    iter:=IteratorByFunctions(rec(

      schutz:=List(SchutzenbergerGroup(d), x-> Representative(d)*x),

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

        return iter!.schutz[at[2]]/OrbMultipliers(d)[OrbSCC(d!.o)[d!.data[1]][at[1]]];
      end,

      ShallowCopy:=iter -> rec(schutz:=iter!.schutz, m:=iter!.m, n:=iter!.n, 
      at:=[0,1])));
  fi;

  SetIsIteratorOfRClassElements(iter, true);
  return iter;
end);

# new for 0.7! - IteratorOfDClassData - "for part perm inverse semigroup""
###############################################################################
# JDM this should have a method like IteratorOfRClassData and
# IteratorOfLClassData since the scc:=OrbSCC line makes this work not so
# well!

InstallMethod(IteratorOfDClassData, "for a part perm inverse semigroup",
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
                 
    o:=o, m:=offset, scc_limit:=Length(scc),

    IsDoneIterator:=iter-> iter!.m=iter!.scc_limit,

    NextIterator:=function(iter)
      local m, o, scc, mults;
      m:=iter!.m; 

      if m=iter!.scc_limit then 
        return fail;
      fi;
     
      o:=iter!.o;   scc:=OrbSCC(o); 
      m:=m+1;       iter!.m:=m;

      mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
      
      return [s, [m], LongOrb(s),
       PartialPermNC(o[scc[m][1]], o[scc[m][1]])];
    end,

    ShallowCopy:=iter-> rec(o:=iter!.o, m:=iter!.m, at:=[0,1],
    scc_limit:=iter!.scc_limit, at_limit:=iter!.at_limit)));
end);

# new for 0.7! - IteratorOfDClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfDClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasDClassReps(s) then 
    return IteratorList(DClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s), x-> x[4],
   [IsIteratorOfDClassReps]);
end);

# new for 0.7! - IteratorOfHClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfDClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasGreensDClasses(s) then 
    return IteratorList(GreensDClasses(s));
  fi;
  return IteratorByIterator(IteratorOfDClassData(s), x->
   CallFuncList(CreateDClass, x), [IsIteratorOfDClasses]);
end);


# new for 0.7! - IteratorOfHClassData - "for part perm inverse semigroup""
###############################################################################
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

# new for 0.7! - IteratorOfHClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfHClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasHClassReps(s) then 
    return IteratorList(HClassReps(s));
  fi;
  return IteratorByIterator(IteratorOfHClassData(s), x-> x[4],
   [IsIteratorOfHClassReps]);
end);

# new for 0.7! - IteratorOfHClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfHClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  if HasGreensHClasses(s) then 
    return IteratorList(GreensHClasses(s));
  fi;
  return IteratorByIterator(IteratorOfHClassData(s), x->
   CallFuncList(CreateHClass, x), [IsIteratorOfHClasses]);
end);

# new for 0.7! - IteratorOfLClassData - "for part perm inverse semigroup""
###############################################################################

InstallMethod(IteratorOfLClassData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, iter, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  if not IsClosed(LongOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(
      
      o:=LongOrb(s), 

      i:=offset,

      IsDoneIterator:=iter-> IsClosed(iter!.o) and iter!.i>=Length(iter!.o),

      NextIterator:=function(iter)
        local i, o, r;
        
        o:=iter!.o; i:=iter!.i;

        if IsClosed(o) and i>=Length(o) then 
          return fail;  
        fi;
        
        i:=i+1;
        
        if i>Length(o) then 
          if not IsClosed(o) then 
            Enumerate(o, i);
            if i>Length(o) then 
              return fail;
            fi;
          else 
            return fail;
          fi;
        fi;

        iter!.i:=i; 
  
        return [s, [1,1,1], ShortOrb(s, o[i]), PartialPermNC(o[i], o[i])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i)));
  else ####

    o:=LongOrb(s); scc:=OrbSCC(o);

    iter:=IteratorByFunctions( rec(
                 
      o:=o,

      m:=offset+1, i:=0,      

      scc_limit:=Length(scc),
      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, m, o, scc, mults;

        i:=iter!.i; m:=iter!.m; 
        if i=iter!.i_limit and m=iter!.scc_limit then
          return fail; 
        fi;

        o:=iter!.o; scc:=OrbSCC(o);
        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;
        iter!.i:=i; iter!.m:=m;
  
        mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
        return [s, [m, scc[m][1], scc[m][i]], LongOrb(s),
         RestrictedPP(mults[scc[m][i]]^-1, o[scc[m][1]])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i, m:=iter!.m,
       scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfLClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfLClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassData(s), x-> x[4],
[IsIteratorOfLClassReps]));

# new for 0.7! - IteratorOfLClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfLClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfLClassData(s), x->
CallFuncList(CreateLClass, x), [IsIteratorOfLClasses]));

# new for 0.7! - IteratorOfRClassData - "for part perm inverse semigroup""
###############################################################################

InstallMethod(IteratorOfRClassData, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup], 
function(s)
  local offset, iter, o, scc;

  if IsPartialPermMonoid(s) then
    offset:=0;
  else
    offset:=1;
  fi; 

  if not IsClosed(LongOrb(s)) then 
    
    iter:=IteratorByFunctions( rec(
      
      o:=LongOrb(s), 

      i:=offset,

      IsDoneIterator:=iter-> IsClosed(iter!.o) and iter!.i>=Length(iter!.o),

      NextIterator:=function(iter)
        local i, o, r;
        
        o:=iter!.o; i:=iter!.i;

        if IsClosed(o) and i>=Length(o) then 
          return fail;  
        fi;
        
        i:=i+1;
        
        if i>Length(o) then 
          if not IsClosed(o) then 
            Enumerate(o, i);
            if i>Length(o) then 
              return fail;
            fi;
          else 
            return fail;
          fi;
        fi;

        iter!.i:=i; 
  
        return [s, [1,1,1], ShortOrb(s, o[i]), PartialPermNC(o[i], o[i])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i)));
  else ####

    o:=LongOrb(s); scc:=OrbSCC(o);

    iter:=IteratorByFunctions( rec(
                 
      o:=o,

      m:=offset+1, i:=0,      

      scc_limit:=Length(scc),
      i_limit:=Length(scc[Length(scc)]),

      IsDoneIterator:=iter-> iter!.m=iter!.scc_limit and 
       iter!.i=iter!.i_limit,

      NextIterator:=function(iter)
        local i, o, m, scc, f, r, mults;
        
        i:=iter!.i; m:=iter!.m; 
        if i=iter!.i_limit and m=iter!.scc_limit then
          return fail; 
        fi;

        o:=iter!.o; scc:=OrbSCC(o);
        if i<Length(scc[m]) then 
          i:=i+1;
        else
          i:=1; m:=m+1;
        fi;
        iter!.i:=i; iter!.m:=m;
  
        mults:=CreateOrbSCCMultipliers(o!.gens, o, m, scc[m]); 
        return [s, [m, scc[m][i], scc[m][1]], LongOrb(s),
         RestrictedPP(mults[scc[m][i]], o[scc[m][i]])];
      end,

      ShallowCopy:=iter-> rec(o:=iter!.o, i:=iter!.i, m:=iter!.m,
       scc_limit:=iter!.scc_limit, i_limit:=iter!.i_limit)));
  fi;
  
  return iter;
end);

# new for 0.7! - IteratorOfRClassReps - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfRClassReps, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x-> x[4],
[IsIteratorOfRClassReps]));

# new for 0.7! - IteratorOfRClasses - "for a part perm inverse semigroup"
###############################################################################

InstallMethod(IteratorOfRClasses, "for a part perm inverse semigroup",
[IsPartialPermSemigroup and IsInverseSemigroup],
s-> IteratorByIterator(IteratorOfRClassData(s), x->
CallFuncList(CreateRClass, x), [IsIteratorOfRClasses]));

#LLL

# new for 0.7! - LClassOfHClass - "for an H-class of an inverse semigrp"
#############################################################################

InstallMethod(LClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateLClass(h!.parent, [data[1], scc[data[1]][1], data[3]], h!.o, 
   mults[data[2]]^-1*Representative(h));
end);

# new for 0.7! - LClassReps - for an inv semi of part perms
##############################################################################

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

# new for 0.7! - LClassReps - for D-class of part perm inv semi
##############################################################################

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

# new for 0.7! - LClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(LClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);

  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensLClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#NNN

# currently no IsInverseActingSemigroupGreensClass exists to use the following
# method; KEEP

InstallOtherMethod(NrLClasses, "for D-class of semigp of partial perms",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and                        IsGreensClassOfInverseSemigroup], NrRClasses);


#PPP

# new for 0.7! - PartialOrderOfDClasses - "for a partial perm inv semigroup"
##############################################################################

InstallMethod(PartialOrderOfDClasses, "for an inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  local d, n, out, gens, o, lookup, offset, i, x, f;
  
  d:=GreensDClasses(s);
  n:=Length(d);
  out:=List([1..n], x-> EmptyPlist(n));
  o:=LongOrb(s);
  gens:=o!.gens;
  lookup:=OrbSCCLookup(o);

  if IsPartialPermMonoid(s) then 
    offset:=0;
  else
    offset:=1;
  fi;

  for i in [1..n] do 
    for x in gens do 
      for f in RClassReps(d[i]) do 
        AddSet(out[i], lookup[Position(o, DomPP(x*f))]-offset);
        AddSet(out[i], lookup[Position(o, RanSetPP(f^-1*x))]-offset);
      od;
    od;
  od;

  Perform(out, ShrinkAllocationPlist);
  return out;
end);

# new for 0.7! - ParentAttr - "for a Green's class of a part perm semigroup
##############################################################################

InstallMethod(ParentAttr, "for a R-class of inverse semigroup",
[IsGreensClass and IsGreensClassOfPartPermSemigroup], x-> x!.parent);

#RRR

# new for 0.7! - Random - "for a part. perm. inv. semigroup (citrus pkg)"
#############################################################################

InstallMethod(Random, "for a part perm inv semigroup (citrus pkg)",
[IsPartialPermSemigroup and IsInverseSemigroup],
function(s)
  local o, gens, i, w, k, m, l, g;

  o:=LongOrb(s);

  if not IsClosed(o) then  
    gens:=GeneratorsOfSemigroup(s);
    i:=Random([1..Int(Length(gens)/2)]);
    w:=List([1..i], x-> Random([1..Length(gens)]));
    return EvaluateWord(gens, w);
  else
    k:=Random([1..Length(o)]);
    m:=OrbSCCLookup(o)[k];
    l:=Random(OrbSCC(o)[m]);
    g:=Random(CreateOrbSCCSchutzGp(o,m)[2]);
    return o!.mults[k]*g/o!.mults[l];
  fi;
end);

# new for 0.7! - RClassOfHClass - "for an H-class of an inverse semigrp"
#############################################################################

InstallMethod(RClassOfHClass, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  local o, scc, data, mults;

  o:=h!.o; scc:=OrbSCC(o); data:=h!.data;
  mults:=OrbMultipliers(h);

  return CreateRClass(h!.parent, [data[1], data[2], scc[data[1]][1]], h!.o, 
   Representative(h)*mults[data[3]]);
end);

# new for 0.7! - RClassReps - for an inv semi of part perms
##############################################################################

InstallOtherMethod(RClassReps, "for an inv semi of part perms",
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
      out[l]:=mults[k]*f;
    od;
  od;
  return out;
end);

# new for 0.7! - RClassReps - for D-class of part perm inv semi
##############################################################################

InstallOtherMethod(RClassReps, "for D-class of part perm inv semi",
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
    out[i]:=mults[j]*rep;
  od;
  return out;
end);

# new for 0.7! - RClassType - "for a partial perm inverse semigroup"
############################################################################

InstallOtherMethod(RClassType, "for a partial perm inverse semigroup",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s);
  return NewType( FamilyObj( s ), IsEquivalenceClass and
         IsEquivalenceClassDefaultRep and IsGreensRClass and
         IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup);
end);

#SSS

# new for 0.7! - Size - for an D-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an D-class of an inverse semigroup",
[IsGreensDClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(d)
  return Length(OrbSCC(d!.o)[d!.data[1]])^2*Size(SchutzenbergerGroup(d));
end);

# new for 0.7! - Size - for an H-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an H-class of an inverse semigroup",
[IsGreensHClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(h)
  return Size(SchutzenbergerGroup(h));
end);

# new for 0.7! - Size - for an L-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an L-class of an inverse semigroup",
[IsGreensLClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(l)
  return Length(OrbSCC(l!.o)[l!.data[1]])*Size(SchutzenbergerGroup(l));
end);

# new for 0.7! - Size - for an R-class of an inverse semigroup
##############################################################################

InstallMethod(Size, "for an R-class of an inverse semigroup",
[IsGreensRClass and IsGreensClassOfPartPermSemigroup and IsGreensClassOfInverseSemigroup],
function(r)
  return Length(OrbSCC(r!.o)[r!.data[1]])*Size(SchutzenbergerGroup(r));
end);

# new for 0.7! - Size - for an inverse semigroup of partial perms
##############################################################################

InstallMethod(Size, "for an inverse semigp of partial perms",
[IsInverseSemigroup and IsPartialPermSemigroup],
function(s)
  EnumerateInverseSemiData(s);
  return Size(s);
end); 

# new for 0.7! - StructureDescription - for a group H-class of inv semi
##############################################################################

InstallOtherMethod(StructureDescription, "for group H-class of inv semi",
[IsGroupHClass and IsGreensClassOfPartPermSemigroup], 
h-> StructureDescription(Range(IsomorphismPermGroup(h))));

#EOF



