#############################################################################
##
#W  enums.gi
#Y  Copyright (C) 2013                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# technical...

InstallGlobalFunction(EnumeratorByEnumerator, 
function(obj, baseenum, convert_out, convert_in, filts, record)
  local enum, filt;
  
  if not (IsDomain(obj) or IsCollectionFamily(obj)) then 
    Error("usage: <obj> must be a domain or a collections family,");
    return;
  elif not (IsEnumeratorByFunctions(baseenum) or IsList(baseenum)) then 
    Error("usage: <baseenum> must be an enumerator or a list,");
    return;
  elif not (IsFunction(convert_out) and IsFunction(convert_in)) then 
    Error("usage: <convert_out> and <convert_in> must be functions,");
    return;
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then 
    Error("usage: <filts> must be a list of filters,");
    return;
  elif not (IsRecord(record) and IsMutable(record) and not
  IsBound(record.baseenum) and not IsBound(record.convert_out) and not
  IsBound(record.convert_in) and not IsBound(record.NumberElement) and not
  IsBound(record.ElementNumber)) then 
    Error("usage: <record> must be a mutable record with no components ",
    " named:\n`baseenum', `convert_out', `convert_in', `ElementNumber',",
    " or `NumberElement',");
    return;
  fi;
  
  record.baseenum:=baseenum;
  record.convert_out:=convert_out;
  record.convert_in:=convert_in;
  
  #
  record.NumberElement:=function(enum, elt)
    return Position(enum!.baseenum, enum!.convert_in(enum, elt));
  end;
  #
  record.ElementNumber:=function(enum, nr)
    return enum!.convert_out(enum, enum!.baseenum[nr]);
  end;
  #
  if not IsBound(record.Length) then 
    record.Length:=enum-> Length(enum!.baseenum);
  fi;
  #
  if IsEnumeratorByFunctions(baseenum) then 
    
    if IsBound(baseenum!.Membership) then 
      record.Membership:=function(enum, elt)
        return enum!.convert_in(enum, elt) in enum!.baseenum;
      end;
    fi; 

    if IsBound(baseenum!.IsBound\[\]) then 
      record.IsBound\[\]:=function(enum, nr)
        return IsBound(enum!.baseenum[nr]);
      end;
    fi;
  
  fi;
  #
  enum:=EnumeratorByFunctions(obj, record);

  for filt in filts do #filters
    SetFilterObj(enum, filt);
  od; 
  return enum;
end);

#

InstallGlobalFunction(EnumeratorByEnumOfEnums,
function(obj, record, baseenum, convert, filts)
  local enum, filt;

  if not (IsDomain(obj) or IsCollectionFamily(obj)) then 
    Error("usage: <obj> must be a domain or a collections family,");
    return;
  elif not IsRecord(record) or IsBound(record.ElementNumber) 
   or IsBound(record.NumberElement) or IsBound(record.baseenum) 
   or IsBound(record.enumofenums) then 
    Error("usage: <record> must be a record with no components named:\n ", 
    "`NumberElement', `ElementNumber', `baseenum', or `enumofenums',");
    return;
  elif not IsFunction(convert) then 
    Error("usage: <convert> must be functions,");
    return;
  elif not (IsList(filts) and ForAll(filts, IsFilter)) then 
    Error("usage: <filts> must be a list of filters,");
    return;
  fi;

  record.baseenum:=baseenum;
  record.enumofenums:=EmptyPlist(Length(baseenum));
  
  if not IsBound(record.Length) then #maybe a better way is in record.Length...
    record.Length:=function(enum)
      local tot, enumofenums, baseenum, i;
      tot:=0;  
      enumofenums:=enum!.enumofenums;
      baseenum:=enum!.baseenum;
      for i in [1..Length(baseenum)] do 
        if not IsBound(enumofenums[i]) then 
          enumofenums[i]:=Enumerator(baseenum[i]);
        fi;
        tot:=tot+Length(enumofenums[i]);
      od;
      return tot;
    end;
  fi;
  #
  record.ElementNumber:=function(enum, pos)
    local enumofenums, baseenum, i;
    
    enumofenums:=enum!.enumofenums;
    baseenum:=enum!.baseenum;

    i:=1; 
    if not IsBound(enumofenums[1]) then 
      enumofenums[1]:=Enumerator(baseenum[1]);
    fi;

    while pos>Length(enumofenums[i]) do 
      pos:=pos-Length(enumofenums[i]);
      i:=i+1;
     if not IsBound(enumofenums[i]) then 
        enumofenums[i]:=Enumerator(baseenum[i]);
      fi;
    od;
    return enumofenums[i][pos];
  end;
  #
  record.NumberElement:=function(enum, elt)
    local baseenum, enumofenums, basepos, pos, i;
    
    baseenum:=enum!.baseenum;
    enumofenums:=enum!.enumofenums;

    basepos:=Position(baseenum, convert(enum, elt));
    if not IsBound(enumofenums[basepos]) then 
      enumofenums[basepos]:=Enumerator(baseenum[basepos]);
    fi;
    pos:=Position(enumofenums[basepos], elt);
    for i in [1..basepos-1] do
      if not IsBound(enumofenums[i]) then 
        enumofenums[i]:=Enumerator(baseenum[i]);
      fi;
      pos:=pos+Length(enumofenums[i]);
    od;
    return pos;
  end;
  #
  enum:=EnumeratorByFunctions(obj, record);
  for filt in filts do 
    SetFilterObj(enum, filt);
  od;
  return enum;
end);

# the actual methods used...

# same method for regular/inverse, note that at present EnumeratorOfRClasses
# does not work for regular or inverse semigrous JDM

InstallMethod(Enumerator, "for an acting semigroup", 
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
function(s)
  local record, convert;

  record:=rec();
  record.Length:=x-> Size(s);
  record.Membership:=function(x, enum)
    return x in enum!.parent;
  end;
  record.parent:=s;

  convert:=function(enum, x)
    return GreensRClassOfElement(enum!.parent, x);
  end;

  return EnumeratorByEnumOfEnums(s, record, EnumeratorOfRClasses(s), 
   convert, []);
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

# same method for regular/inverse

#JDM this should be improved, using Iterator for a regular or inverse
#semigroup, invokes IteratorOfRClassData which repeatedly recomputes the graded
#lambda orbs of the R-class reps.

InstallMethod(EnumeratorSorted, "for an acting semigroup", 
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
function(s)
  return Immutable(SSortedList(ListIterator(Iterator(s), Size(s))));
end);

# different method for inverse

InstallMethod(Enumerator, "for a regular D-class of an acting semigroup",
[IsGreensDClass and IsRegularClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, rho_scc, lambda_scc;
  
  Enumerate(LambdaOrb(d), infinity);
  Enumerate(RhoOrb(d), infinity);

  record:=rec(parent:=d);
  
  record.Membership:=function(elt, enum)
    return elt in d;
  end;
  #
  convert_out:=function(enum, tuple)
    local d, rep;
    d:=enum!.parent; rep:=Representative(d);
    return RhoOrbMult(RhoOrb(d), RhoOrbSCCIndex(d), tuple[1])[1]*rep*tuple[2]
     *LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[3])[1];
  end;
  #
  convert_in:=function(enum, elt)
    local d, s, k, l, f;
    
    d:=enum!.parent;
    s:=Parent(d); 
    
    k:=Position(RhoOrb(d), RhoFunc(s)(elt));
    l:=Position(LambdaOrb(d), LambdaFunc(s)(elt));
    
    f:=RhoOrbMult(RhoOrb(d), RhoOrbSCCIndex(d), k)[2]*elt
     *LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), l)[2];
    
    return [k, LambdaPerm(s)(Representative(d), f), l];
  end;
  #
  rho_scc:=OrbSCC(RhoOrb(d))[RhoOrbSCCIndex(d)];
  lambda_scc:=OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
  #
  return EnumeratorByEnumerator(d, 
    EnumeratorOfCartesianProduct(rho_scc, SchutzenbergerGroup(d), lambda_scc),
    convert_out, convert_in, [], record);
end);

#

InstallMethod(Enumerator, "for a D-class of an inverse acting semigroup",
[IsGreensDClass and IsInverseOpClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, lambda_scc;
  
  Enumerate(LambdaOrb(d), infinity);

  record:=rec(parent:=d);
  #
  convert_out:=function(enum, tuple)
    local d, rep;
    d:=enum!.parent; rep:=Representative(d);
    return LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[1])[2]
     *rep*tuple[2]
     *LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[3])[1];
  end;
  #
  convert_in:=function(enum, elt)
    local d, s, k, l, f;
    
    d:=enum!.parent;
    s:=Parent(d); 
    
    k:=Position(LambdaOrb(d), RhoFunc(s)(elt));
    l:=Position(LambdaOrb(d), LambdaFunc(s)(elt));
    
    f:=LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), k)[1]*elt
     *LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), l)[2];
    
    return [k, LambdaPerm(s)(Representative(d), f), l];
  end;
  #
  lambda_scc:=OrbSCC(LambdaOrb(d))[LambdaOrbSCCIndex(d)];
  #
  return EnumeratorByEnumerator(d, 
    EnumeratorOfCartesianProduct(lambda_scc, SchutzenbergerGroup(d),
     lambda_scc), convert_out, convert_in, [], record);
end);
#      # p = a^-1*b where a in cosets and b in lschutz
#      p:=LambdaPerm(s)(rep, g);
#      
#      if schutz=true then # a=() and so p=b
#        j:=1;
#      elif schutz=false then # b=() and so p=a^-1 in cosets
#        j:=PositionCanonical(cosets, p^-1);
#      else
#        for j in [1..Length(cosets)] do
#         #if SiftGroupElement(schutz, g*cosets[j]).isone then 
#    
#
#    return EnumeratorByFunctions(d, rec(
#
#    m:=Length(LambdaOrbSCC(d))*Size(LambdaOrbSchutzGp(LambdaOrb(d),
#     LambdaOrbSCCIndex(d))),
#    # size of any R-class in d.
#
#    ElementNumber:=function(enum, pos)
#    local q, n, m, R;
#
#      if pos>Length(enum) then
#        return fail;
#      fi;
#
#      R:=GreensRClasses(d);
#      n:=pos-1;
#      m:=enum!.m;
#
#      q := QuoInt(n, m);
#      pos:= [ q, n - q * m ]+1;
#      return Enumerator(R[pos[1]])[pos[2]];
#    end,
#
#    #######################################################################
#
#    NumberElement:=function(enum, f)
#      local s, rep, g, lm, lo, lscc, ll, lschutz, rm, ro, rscc, rl, schutz,
#      cosets, j, r, p; 
#
#      s:=Parent(d);
#      rep:=Representative(d);
#
#      if ElementsFamily(FamilyObj(s)) <> FamilyObj(f) 
#        or ActionRank(f) <> ActionRank(rep) then
#        return fail;
#      fi;
#
#      lm:=LambdaOrbSCCIndex(d); lo:=LambdaOrb(d); lscc:=OrbSCC(lo);
#      ll:=Position(lo, LambdaFunc(s)(f));
#
#      if ll = fail or OrbSCCLookup(lo)[ll]<>lm then
#        return fail;
#      fi;
#     
#      if ll<>lscc[lm][1] then
#        f:=f*LambdaOrbMult(lo, lm, ll)[2];
#      fi;
#      g:=f;
#
#      lschutz:=Enumerator(LambdaOrbSchutzGp(lo, lm));
#
#      rm:=RhoOrbSCCIndex(d); ro:=RhoOrb(d); rscc:=OrbSCC(ro);
#      rl:=Position(ro, RhoFunc(s)(g));
#
#      if rl = fail or OrbSCCLookup(ro)[rl]<>rm then
#        return fail;
#      fi;
#      
#      if rl<>rscc[rm][1] then
#        g:=RhoOrbMult(ro, rm, rl)[2]*f;
#      fi;
#
#      schutz:=LambdaOrbStabChain(lo, lm);
#      cosets:=RhoCosets(d);
#      # p = a^-1*b where a in cosets and b in lschutz
#      p:=LambdaPerm(s)(rep, g);
#      
#      if schutz=true then # a=() and so p=b
#        j:=1;
#      elif schutz=false then # b=() and so p=a^-1 in cosets
#        j:=PositionCanonical(cosets, p^-1);
#      else
#        for j in [1..Length(cosets)] do
#          #if SiftGroupElement(schutz, g*cosets[j]).isone then 
#          if SiftedPermutation(schutz, cosets[j]*p)=() then 
#            break;
#          else
#            j:=fail;
#          fi;
#        od;
#      fi;
#      
#      if j=fail then 
#        return fail;
#      fi;
#
#      #JDM better to avoid the Position in the next line (which is essential)
#      r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
#      return enum!.m*r+Length(lschutz)*(Position(lscc[lm], ll)-1)+
#      Position(lschutz, cosets[j]*p);
#    end,
#
#    #######################################################################
#    
#    Membership:=function(elm, enum)
#      return elm in d;
#    end,
#
#    Length:=enum -> Size(d),
#
#    PrintObj:=function(enum)
#      Print( "<enumerator of D-class>");
#    return;
#  end));
#end);

# same method for inverse/regular

InstallMethod(Enumerator, "for H-class of acting semigp.",
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

# different method for inverse

#JDM use these for enumerator of symmetric inverse semigroup
# using EnumeratorByEnumerator

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

#

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

#

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

  convert_out:=function(enum, x)
    return ArrangementNumber(x, m, n);
  end;

  convert_in:=function(enum, x)
    return NumberArrangement(x, n);
  end;

  fam:=CollectionsFamily(FamilyObj(ArrangementNumber(1, m, n)));
  
  return EnumeratorByEnumerator(fam, 
   Enumerator([1..NrArrangements([1..n], m)]), convert_out, convert_in, [],
    rec());
end);
#EOF
