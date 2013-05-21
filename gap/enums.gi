#############################################################################
##
#W  enums.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

InstallGlobalFunction(NumberArrangement, 
function(arr, n)
  local bool, m, mult, factor, out, k, i, j;
  
  if IsEmpty(arr) then 
    return 1;
  fi;

  bool:=BlistList([1..n], []);
  m:=Length(arr);

  bool[arr[1]]:=true;
  mult:=Product([n-m+1..n-1]);
  factor:=n;
  out:=(arr[1]-1)*mult;

  for i in [2..m] do 
    k:=0;
    for j in [1..arr[i]-1] do 
      if not bool[j] then k:=k+1; fi;
    od;
    bool[arr[i]]:=true;
    factor:=factor-1;
    mult:=mult/factor;
    out:=out+k*mult;
  od;
  return out+1;
end);

InstallGlobalFunction(ArrangementNumber, 
function(r, m, n)
  local bool, mult, factor, q, out, j, k, i;
  if m=0 then 
    return [];
  fi;
  if m=1 then 
    return [r];
  fi;
  bool:=BlistList([1..n], []);
  r:=r-1;
  mult:=Product([n-m+1..n-1]); 
  factor:=n; 
  q:=QuotientRemainder(r,mult);
  out:=[q[1]+1];
  bool[q[1]+1]:=true;
  
  for i in [2..m-1] do  
    factor:=factor-1;
    mult:=mult/factor;
    q:=QuotientRemainder(q[2], mult);
    j:=0; k:=0; 
    repeat 
      j:=j+1;
      if not bool[j] then k:=k+1; fi;
    until k=q[1]+1;
    bool[j]:=true;
    out[i]:=j;
  od;
  j:=0; k:=0;
  repeat
    j:=j+1;
    if not bool[j] then k:=k+1; fi;
  until k=q[2]+1;
  out[m]:=j;
  return out;
end);

#JDM use these for enumerator of symmetric inverse semigroup
# using EnumeratorByEnumerator

InstallGlobalFunction(EnumeratorByEnumerator, 
function(domain, baseenum, convert_out, convert_in, filts)
  local record, enum, filt;
  
  record:=rec(baseenum:=baseenum, convert_out:=convert_out, 
   convert_in:=convert_in);

  record.NumberElement:=function(enum, elt)
    return Position(enum!.baseenum, enum!.convert_in(elt));
  end;

  record.ElementNumber:=function(enum, nr)
    return enum!.convert_out(enum!.baseenum[nr]);
  end;
  
  record.Length:=enum-> Length(enum!.baseenum);
  
  if IsEnumeratorByFunctions(baseenum) then 
    
    if IsBound(baseenum!.Membership) then 
      record.Membership:=function(enum, elt)
        return enum!.convert_in(elt) in enum!.baseenum;
      end;
    fi; 

    if IsBound(baseenum!.IsBound\[\]) then 
      record.IsBound\[\]:=function(enum, nr)
        return IsBound(enum!.baseenum[nr]);
      end;
    fi;
  
  fi;

  enum:=EnumeratorByFunctions(domain, record);

  for filt in filts do #filters
    SetFilterObj(enum, filt);
  od; 
  return enum;
end);

InstallGlobalFunction(EnumeratorOfArrangements, 
function(m, n)
  local convert_out, convert_in, fam;

  if not IsPosInt(n) then
    Error("usage: <n> must be a positive integer,");
    return;
  elif not (IsInt(m) and m>=0) then
    Error("usage: <m> must be a non-negative integer,");
    return;
  elif m>n then
    Error("usage: <m> must be no greater than <n>,");
  fi;

  convert_out:=function(x)
    return ArrangementNumber(x, m, n);
  end;

  convert_in:=function(x)
    return NumberArrangement(x, n);
  end;

  fam:=CollectionsFamily(FamilyObj(ArrangementNumber(1, m, n)));
  
  return EnumeratorByEnumerator(fam, 
   Enumerator([1..NrArrangements([1..n], m)]), convert_out, convert_in, []);
end);

# Notes: 
# this is not an enumerator as I could not get an enumerator to perform 
# well here. 

# same method for regular/inverse

#JDM this should be improved, using Iterator for a regular or inverse
#semigroup, invokes IteratorOfRClassData which repeatedly recomputes the graded
#lambda orbs of the R-class reps.

InstallMethod(Enumerator, "for an acting semigroup", 
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
function(s)
  local out, iter, j, i;

  out:=EmptyPlist(Size(s)); 

  iter:=Iterator(s); 
  j:=0;

  for i in iter do 
    j:=j+1;
    out[j]:=i;
  od;

  return Immutable(out);
end);

# different method for inverse/regular

InstallOtherMethod(Enumerator, "for a D-class of acting semigp.",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
    
    Enumerate(LambdaOrb(d), infinity);
    Enumerate(RhoOrb(d), infinity);

    return EnumeratorByFunctions(d, rec(

    m:=Length(LambdaOrbSCC(d))*Size(LambdaOrbSchutzGp(LambdaOrb(d),
     LambdaOrbSCCIndex(d))),
    # size of any R-class in d.

    ElementNumber:=function(enum, pos)
    local q, n, m, R;

      if pos>Length(enum) then
        return fail;
      fi;

      R:=GreensRClasses(d);
      n:=pos-1;
      m:=enum!.m;

      q := QuoInt(n, m);
      pos:= [ q, n - q * m ]+1;
      return Enumerator(R[pos[1]])[pos[2]];
    end,

    #######################################################################

    NumberElement:=function(enum, f)
      local s, rep, g, lm, lo, lscc, ll, lschutz, rm, ro, rscc, rl, schutz,
      cosets, j, r, p; 

      s:=Parent(d);
      rep:=Representative(d);

      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
        or ActionRank(f) <> ActionRank(rep) then
        return fail;
      fi;

      lm:=LambdaOrbSCCIndex(d); lo:=LambdaOrb(d); lscc:=OrbSCC(lo);
      ll:=Position(lo, LambdaFunc(s)(f));

      if ll = fail or OrbSCCLookup(lo)[ll]<>lm then
        return fail;
      fi;
     
      if ll<>lscc[lm][1] then
        f:=f*LambdaOrbMult(lo, lm, ll)[2];
      fi;
      g:=f;

      lschutz:=Enumerator(LambdaOrbSchutzGp(lo, lm));

      rm:=RhoOrbSCCIndex(d); ro:=RhoOrb(d); rscc:=OrbSCC(ro);
      rl:=Position(ro, RhoFunc(s)(g));

      if rl = fail or OrbSCCLookup(ro)[rl]<>rm then
        return fail;
      fi;
      
      if rl<>rscc[rm][1] then
        g:=RhoOrbMult(ro, rm, rl)[2]*f;
      fi;

      schutz:=LambdaOrbStabChain(lo, lm);
      cosets:=RhoCosets(d);
      # p = a^-1*b where a in cosets and b in lschutz
      p:=LambdaPerm(s)(rep, g);
      
      if schutz=true then # a=() and so p=b
        j:=1;
      elif schutz=false then # b=() and so p=a^-1 in cosets
        j:=PositionCanonical(cosets, p^-1);
      else
        for j in [1..Length(cosets)] do
          #if SiftGroupElement(schutz, g*cosets[j]).isone then 
          if SiftedPermutation(schutz, cosets[j]*p)=() then 
            break;
          else
            j:=fail;
          fi;
        od;
      fi;
      
      if j=fail then 
        return fail;
      fi;

      #JDM better to avoid the Position in the next line (which is essential)
      r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
      return enum!.m*r+Length(lschutz)*(Position(lscc[lm], ll)-1)+
      Position(lschutz, cosets[j]*p);
    end,

    #######################################################################
    
    Membership:=function(elm, enum)
      return elm in d;
    end,

    Length:=enum -> Size(d),

    PrintObj:=function(enum)
      Print( "<enumerator of D-class>");
    return;
  end));
end);

# same method for inverse/regular

InstallOtherMethod(Enumerator, "for H-class of acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  return EnumeratorByFunctions(h, rec(

    schutz:=Enumerator(SchutzenbergerGroup(h)),

    ElementNumber:=function(enum, pos)
      if pos>Length(enum) then
        return fail;
      fi;

      return Representative(h)*enum!.schutz[pos];
    end,

    NumberElement:=function(enum, f)
      local s, rep;
      s:=Parent(h);
      rep:=Representative(h);

      if ActionRank(f) <> ActionRank(rep) 
        or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) 
        or RhoFunc(s)(f) <> RhoFunc(s)(rep) then
        return fail;
      fi;

      return Position(enum!.schutz, LambdaPerm(s)(rep, f));
    end,

    Membership:=function(elm, enum)
      return elm in h; #the H-class itself!
    end,

    Length:=enum -> Size(h),

    PrintObj:=function(enum)
      Print( "<enumerator of H-class>");
      return;

  end));
end);

# same method for regular, different method for inverse

InstallMethod(Enumerator, "for L-class of an acting semigroup",
[IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, mults, scc;

  o:=RhoOrb(l); 
  m:=RhoOrbSCCIndex(l);
  mults:=RhoOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  return EnumeratorByFunctions(l, rec(

    schutz:=Enumerator(SchutzenbergerGroup(l)),

    len:=Size(SchutzenbergerGroup(l)),

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(l)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return mults[scc[pos[1]]][1]*enum[pos[2]];
    end,

    NumberElement:=function(enum, f)
      local s, rep, o, m, i, g, j;

      s:=Parent(l);
      rep:=Representative(l);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
        or ActionDegree(f) <> ActionDegree(rep) 
        or ActionRank(f) <> ActionRank(rep) 
        or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=RhoOrb(l); m:=RhoOrbSCCIndex(l);
      i:=Position(o, RhoFunc(s)(f));

      if i = fail or OrbSCCLookup(o)[i]<>m then 
        return fail;
      fi;
     
      j:=Position(enum!.schutz, LambdaPerm(s)(rep, mults[i][2]*f));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, i)-1)+j;
    end,

    Membership:=function(elm, enum)
      return elm in l;
    end,

    Length:=enum-> Size(l),

    PrintObj:=function(enum)
      Print("<enumerator of L-class>");
      return;
    end));
end);

# same method for regular/inverse

InstallMethod(Enumerator, "for R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local o, m, mults, scc;

  o:=LambdaOrb(r); 
  m:=LambdaOrbSCCIndex(r);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  return EnumeratorByFunctions(r, rec(

    schutz:=Enumerator(SchutzenbergerGroup(r)),

    len:=Size(SchutzenbergerGroup(r)),

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(r)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return enum[pos[2]]*mults[scc[pos[1]]][1];
    end,

    NumberElement:=function(enum, f)
      local s, rep, o, m, l, g, j;

      s:=Parent(r);
      rep:=Representative(r);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       ActionDegree(f) <> ActionDegree(rep) or ActionRank(f) <> ActionRank(rep)
       or RhoFunc(s)(f) <> RhoFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=LambdaOrb(r); m:=LambdaOrbSCCIndex(r);
      l:=Position(o, LambdaFunc(s)(f));

      if l = fail or OrbSCCLookup(o)[l]<>m then 
        return fail;
      fi;
      g:=f;
      if l<>OrbSCC(o)[m][1] then  
        g:=g*mults[l][2];
      fi;
      j:=Position(enum!.schutz, LambdaPerm(s)(rep, g));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, l)-1)+j;
    end,

    Membership:=function(elm, enum)
      return elm in r;
    end,

    Length:=enum-> Size(r),

    PrintObj:=function(enum)
      Print("<enumerator of R-class>");
      return;
    end));
end);

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

# no method for regular/inverse semigroup just yet, JDM

InstallMethod(EnumeratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
function(s)
  local enum;

  return EnumeratorByFunctions(s, rec(
    
    ElementNumber:=function(enum, pos)
      return GreensRClasses(s)[pos];
    end,

    NumberElement:=function(enum, r)
      return Position(SemigroupData(s), Representative(r))-1;
    end,

    Membership:=function(r, enum)
      return Position(enum, r)<>fail;
    end,
    
    Length:=enum -> NrRClasses(s),

    PrintObj:=function(enum)
      Print( "<enumerator of R-classes>");
      return;
    end));

  return enum;
end);

# JDM why is IsGreensClassOfPartPermSemigroup still used here!?

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

      scc:=LambdaOrbSCC(d);
      mults:=LambdaOrbMults(LambdaOrb(d), LambdaOrbSCCIndex(d));

      n:=pos-1; m:=Length(enum!.schutz); r:=Length(scc);
      q:=QuoInt(n, m); q2:=QuoInt(q, r);
      pos:=[ n-q*m, q2, q  - q2 * r ]+1;
      return mults[scc[pos[2]]]*enum[pos[1]]/mults[scc[pos[3]]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local rep, o, m, lookup, s, i, j, scc, g, k;

      rep:=Representative(d);
      
      if ActionRank(f)<>ActionRank(rep) or ActionDegree(f)<>ActionDegree(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=LambdaOrb(d); m:=LambdaOrbSCCIndex(d);
      lookup:=OrbSCCLookup(o);
      s:=Parent(d);

      i:=Position(o, RhoFunc(s)(f)); 
      if i=fail or not lookup[i]<>m then 
        return fail;
      fi;

      j:=Position(o, LambdaFunc(s)(f));
      if j=fail or not lookup[j]<>m then 
        return fail;
      fi;

      scc:=OrbSCC(o)[m]; g:=f;
      
      if i<>scc[1] then 
        g:=LambdaOrbMult(o, m, i)[1]*g;
      fi;
      
      if j<>scc[1] then 
        g:=g*LambdaOrbMult(o, m, j)[2];
      fi;

      k:=Position(enum!.schutz, LambdaPerm(s)(rep, g));
      if j=fail then 
        return fail;
      fi;

      return Length(enum!.schutz)*((Position(scc, i)-1)*Length(scc)
      +(Position(scc, j)-1))+k;
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


#

InstallMethod(Enumerator, "for L-class of an acting semigroup",
[IsInverseOpClass and IsGreensLClass and IsActingSemigroupGreensClass],
function(l)
  local o, m, mults, scc;

  o:=LambdaOrb(l); 
  m:=LambdaOrbSCCIndex(l);
  mults:=LambdaOrbMults(o, m);
  scc:=OrbSCC(o)[m];

  return EnumeratorByFunctions(l, rec(

    schutz:=Enumerator(SchutzenbergerGroup(l)),

    len:=Size(SchutzenbergerGroup(l)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      local n, m, q;

      if pos>Length(enum) then 
        return fail;
      fi;

      if pos<=Length(enum!.schutz) then 
        return Representative(l)*enum!.schutz[pos];
      fi;

      n:=pos-1; m:=enum!.len;
      
      q:=QuoInt(n, m); 
      pos:=[ q, n - q * m]+1;
     
     return mults[scc[pos[1]]][2]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, i, g, j;

      s:=Parent(l);
      rep:=Representative(l);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       ActionDegree(f) <> ActionDegree(rep) or ActionRank(f) <> ActionRank(rep)
       or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then 
        return fail;
      fi;
      
      if f=rep then 
        return 1;
      fi;

      o:=RhoOrb(l); m:=RhoOrbSCCIndex(l);
      i:=Position(o, RhoFunc(s)(f));

      if i = fail or OrbSCCLookup(o)[i]<>m then 
        return fail;
      fi;
     
      j:=Position(enum!.schutz, LambdaPerm(s)(rep, mults[i][1]*f));

      if j=fail then 
        return fail;
      fi;
      return enum!.len*(Position(scc, i)-1)+j;
    end,

    #########################################################################

    Membership:=function(elm, enum)
      return elm in l;
    end,

    Length:=enum-> Size(l),

    PrintObj:=function(enum)
      Print("<enumerator of L-class>");
      return;
    end));
end);
#EOF
