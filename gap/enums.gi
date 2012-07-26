#############################################################################
##
#W  enums.gi
#Y  Copyright (C) 2011-12                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# mod for 1.0! - Enumerator - "for an acting semigroup"
#############################################################################
# Notes: this is not an enumerator as I could not get an enumerator to perform 
# well here. 

# same method for regular/inverse

InstallMethod(Enumerator, "for an acting semigroup", 
[IsActingSemigroup and HasGeneratorsOfSemigroup], 
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

# mod for 1.0! - Enumerator - "for a D-class of acting semigp."
#############################################################################

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

    #######################################################################

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

      s:=ParentSemigroup(d);
      rep:=Representative(d);

      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or f[2] <> rep[2] then
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

# mod for 1.0! - Enumerator - "for H-class of acting semigp."
#############################################################################

# same method for inverse/regular

InstallOtherMethod(Enumerator, "for H-class of acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  return EnumeratorByFunctions(h, rec(

    schutz:=Enumerator(SchutzenbergerGroup(h)),

    #########################################################################

    ElementNumber:=function(enum, pos)
      if pos>Length(enum) then
        return fail;
      fi;

      return Representative(h)*enum!.schutz[pos];
    end,

    #########################################################################

    NumberElement:=function(enum, f)
      local s, rep;
      s:=ParentSemigroup(h);
      rep:=Representative(h);

      if Rank(f) <> Rank(rep) or LambdaFunc(s)(f) <> LambdaFunc(s)(rep) or
       RhoFunc(s)(f) <> RhoFunc(s)(rep) then
        return fail;
      fi;

      return Position(enum!.schutz, LambdaPerm(s)(rep, f));
    end,

    ###########################################################################

    Membership:=function(elm, enum)
      return elm in h; #the H-class itself!
    end,

    Length:=enum -> Size(h),

    PrintObj:=function(enum)
      Print( "<enumerator of H-class>");
      return;

  end));
end);

# mod for 1.0! - Enumerator - "for L-class of an acting semigroup"
##############################################################################

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
     
     return mults[scc[pos[1]]][1]*enum[pos[2]];
    end,

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, i, g, j;

      s:=ParentSemigroup(l);
      rep:=Representative(l);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       Degree(f) <> Degree(rep) or Rank(f) <> Rank(rep) or 
       LambdaFunc(s)(f) <> LambdaFunc(s)(rep) then 
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

# mod for 1.0! - Enumerator - "for R-class of an acting semigroup"
##############################################################################

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

    #########################################################################

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

    #########################################################################
    
    NumberElement:=function(enum, f)
      local s, rep, o, m, l, g, j;

      s:=ParentSemigroup(r);
      rep:=Representative(r);
      
      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) or 
       Degree(f) <> Degree(rep) or Rank(f) <> Rank(rep) or 
       RhoFunc(s)(f) <> RhoFunc(s)(rep) then 
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

    #########################################################################

    Membership:=function(elm, enum)
      return elm in r;
    end,

    Length:=enum-> Size(r),

    PrintObj:=function(enum)
      Print("<enumerator of R-class>");
      return;
    end));
end);

# mod for 1.0! - EnumeratorOfRClasses - "for an acting semigroup"
#############################################################################
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



