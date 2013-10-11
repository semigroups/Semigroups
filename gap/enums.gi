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

# <convert_in> must return <fail> if it is not possible to convert
# <convert_out> must check if its argument is <fail> and if it is, then it
# should return <fail>, <convert_out> should have two arguments <enum> and <nr>
# where <nr> refers to the position in <baseenum>.

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
    local converted;
    converted:=enum!.convert_in(enum, elt);
    if converted=fail then 
      return fail;
    fi;
    return Position(enum!.baseenum, converted);
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
        local converted; 
        converted:=enum!.convert_in(enum, elt);
        if converted=fail then 
          return false;
        fi;
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
    local baseenum, enumofenums, conv, basepos, pos, i;
    
    baseenum:=enum!.baseenum;
    enumofenums:=enum!.enumofenums;
    conv:=convert(enum, elt); 
    if conv=fail then return fail; fi;
    basepos:=Position(baseenum, conv);
    if basepos=fail then return fail; fi;
    if not IsBound(enumofenums[basepos]) then 
      enumofenums[basepos]:=Enumerator(baseenum[basepos]);
    fi;
    pos:=Position(enumofenums[basepos], elt);
    if pos=fail then return fail; fi;
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

# also this has really awful performance

InstallMethod(Enumerator, "for an acting semigroup", 
[IsActingSemigroup], 5, #to beat the method for semigroup ideals
function(s)
  local record, convert;
  
  if HasAsSSortedList(s) then 
    return AsSSortedList(s);
  fi;
  
  record:=rec();
  record.Length:=x-> Size(s);
  record.Membership:=function(x, enum)
    return x in enum!.parent;
  end;
  record.parent:=s;

  convert:=function(enum, x)
    if x in enum!.parent then 
      return GreensRClassOfElementNC(enum!.parent, x);
    fi;
    return fail;
  end;

  return EnumeratorByEnumOfEnums(s, record, EnumeratorOfRClasses(s), 
   convert, []);
end);

# Notes: the only purpose for this is the method for NumberElement.  Otherwise
# use (if nothing much is known) IteratorOfRClasses or if everything is know
# just use RClasses.

# different method for regular/inverse

# JDM the performance of this also sucks

InstallMethod(EnumeratorOfRClasses, "for an acting semigroup",
[IsActingSemigroup], 
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
      Print( "<enumerator of R-classes of ", ViewString(s), ">");
      return;
    end));

  return enum;
end);

# different method for inverse

InstallMethod(EnumeratorOfRClasses, "for a regular acting semigroup",
[IsActingSemigroup and IsRegularSemigroup], 
function(s)
  local o;

  o:=RhoOrb(s);
  Enumerate(o, infinity);

  return EnumeratorByFunctions(s, rec(
    
    parent:=s,

    Length:=enum-> NrRClasses(enum!.parent), 
    
    Membership:=function(r, enum)
      return Representative(r) in enum!.parent;
    end,

    NumberElement:=function(enum, r)
      local pos; 
      pos:=Position(RhoOrb(enum!.parent),
       RhoFunc(enum!.parent)(Representative(r)));
      if pos=fail then 
        return fail;
      fi;
      return pos-1;  
    end,

   ElementNumber:=function(enum, nr)
    local s, o, m;
    s:=enum!.parent;
    o:=RhoOrb(s);
    m:=OrbSCCLookup(o)[nr+1];
    return CreateRClass(s, m, LambdaOrb(s), 
     RhoOrbMult(o, m, nr+1)[1]*RhoOrbRep(o, m), false);
   end,
   PrintObj:=function(enum)
     Print( "<enumerator of R-classes of ", ViewString(s), ">");
     return;
   end));
end);   

# JDM again this method might not nec. if inverse op semigroups have RhoOrb
# method

InstallMethod(EnumeratorOfRClasses, "for an inverse op acting semigroup",
[IsActingSemigroupWithInverseOp], 
function(s)
  local o;

  o:=LambdaOrb(s);
  Enumerate(o, infinity);

  return EnumeratorByFunctions(s, rec(
    
    parent:=s,

    Length:=enum-> NrRClasses(enum!.parent), 
    
    Membership:=function(r, enum)
      return Representative(r) in enum!.parent;
    end,

    NumberElement:=function(enum, r)
      local pos; 
      pos:=Position(LambdaOrb(enum!.parent),
       RhoFunc(enum!.parent)(Representative(r)));
      if pos=fail then 
        return fail;
      fi;
      return pos-1;  
    end,

   ElementNumber:=function(enum, nr)
    local s, o, m;
    s:=enum!.parent;
    o:=LambdaOrb(s);
    m:=OrbSCCLookup(o)[nr+1];
    return CreateRClassNC(s, m, LambdaOrb(s), 
     LambdaOrbMult(o, m, nr+1)[2]*RightOne(LambdaOrbRep(o, m)), false);
   end,

   PrintObj:=function(enum)
     Print( "<enumerator of R-classes of ", ViewString(s), ">");
     return;
   end));
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

# different method for regular/inverse

# this could be better at the cost of being much more complicated...

InstallMethod(Enumerator, "for a D-class of an acting semigroup",
[IsGreensDClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert;
  
  if HasAsSSortedList(d) then 
    return AsSSortedList(d);
  fi;
  record:=rec(parent:=d);
  
  record.PrintObj:=function(enum)
    Print("<enumerator of D-class>");
  end;
  
  convert:=function(enum, elt)
    return GreensRClassOfElement(enum!.parent, elt);
  end;

  return EnumeratorByEnumOfEnums(d, record, GreensRClasses(d), convert,
   []);
end);

# different method for inverse

InstallMethod(Enumerator, "for a regular D-class of an acting semigroup",
[IsGreensDClass and IsRegularClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, rho_scc, lambda_scc;
   
  if HasAsSSortedList(d) then 
    return AsSSortedList(d);
  fi;
  
  Enumerate(LambdaOrb(d), infinity);
  Enumerate(RhoOrb(d), infinity);

  record:=rec(parent:=d);
  
  record.Membership:=function(elt, enum)
    return elt in d;
  end;
  record.PrintObj:=function(enum)
    Print("<enumerator of D-class>");
  end;

  #
  convert_out:=function(enum, tuple)
    local d, rep, act;
    if tuple=fail then return fail; fi;
    d:=enum!.parent; rep:=Representative(d);
    act:=StabilizerAction(Parent(d));
    return act(RhoOrbMult(RhoOrb(d), RhoOrbSCCIndex(d), tuple[1])[1]*rep,
     tuple[2])*LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[3])[1];
  end;
  #
  convert_in:=function(enum, elt)
    local d, s, k, l, f;
    
    d:=enum!.parent;
    s:=Parent(d); 
     
    k:=Position(RhoOrb(d), RhoFunc(s)(elt));
    if OrbSCCLookup(RhoOrb(d))[k]<>RhoOrbSCCIndex(d) then 
      return fail;
    fi;
    l:=Position(LambdaOrb(d), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[l]<>LambdaOrbSCCIndex(d) then 
      return fail;
    fi;

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

#this method is unnecesary if we write a method for RhoOrb of a inverse op
#D-classJDM

InstallMethod(Enumerator, "for a D-class of an inverse acting semigroup",
[IsGreensDClass and IsInverseOpClass and IsActingSemigroupGreensClass],
function(d)
  local record, convert_out, convert_in, lambda_scc;
  
  if HasAsSSortedList(d) then 
    return AsSSortedList(d);
  fi;
  
  Enumerate(LambdaOrb(d), infinity);

  record:=rec(parent:=d);
  record.PrintObj:=function(enum)
    Print("<enumerator of D-class>");
  end;

  #
  convert_out:=function(enum, tuple)
    local d, rep, act;
    if tuple=fail then return fail; fi;
    d:=enum!.parent; rep:=Representative(d); 
    act:=StabilizerAction(Parent(d));
    return act(LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d), tuple[1])[2]
     *rep, tuple[2])*LambdaOrbMult(LambdaOrb(d), LambdaOrbSCCIndex(d),
     tuple[3])[1];
  end;
  #
  convert_in:=function(enum, elt)
    local d, s, k, l, f;
    
    d:=enum!.parent;
    s:=Parent(d); 
    
    k:=Position(LambdaOrb(d), RhoFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[k]<>LambdaOrbSCCIndex(d) then 
      return fail;
    fi;
    l:=Position(LambdaOrb(d), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(d))[l]<>LambdaOrbSCCIndex(d) then 
      return fail;
    fi;
    
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

# same method for inverse/regular

InstallMethod(Enumerator, "for H-class of acting semigp.",
[IsGreensHClass and IsActingSemigroupGreensClass],
function(h)

  if HasAsSSortedList(h) then 
    return AsSSortedList(h);
  fi;
  
  return EnumeratorByFunctions(h, rec(

    schutz:=Enumerator(SchutzenbergerGroup(h)),

    ElementNumber:=function(enum, pos)
      if pos>Length(enum) then
        return fail;
      fi;

      return StabilizerAction(Parent(h))(Representative(h), enum!.schutz[pos]);
    end,

    NumberElement:=function(enum, f)
      local s, rep;
      s:=Parent(h);
      rep:=Representative(h);

      if LambdaFunc(s)(f) <> LambdaFunc(s)(rep) 
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
  local record, convert_out, convert_in, scc;
  
  if HasAsSSortedList(l) then 
    return AsSSortedList(l);
  fi;
  
  record:=rec(parent:=l);
  record.Membership:=function(elm, enum)
    return elm in l;
  end;
  record.PrintObj:=function(enum) 
    Print("<enumerator of L-class>");
  end;

  record.Length:=enum-> Size(l);
  #
  convert_out:=function(enum, tuple)
    local l, rep, act;
    if tuple=fail then return fail; fi;
    l:=enum!.parent;
    rep:=Representative(l);
    act:=StabilizerAction(Parent(l));
    return act(RhoOrbMult(RhoOrb(l), RhoOrbSCCIndex(l),
      tuple[1])[1]*rep, tuple[2]);
  end;
  #
  convert_in:=function(enum, elt)
    local l, s, i, f;
    l:=enum!.parent;
    s:=Parent(l); 
   
    if LambdaFunc(s)(elt)<>LambdaFunc(s)(Representative(l)) then 
      return fail;
    fi;
    
    i:=Position(RhoOrb(l), RhoFunc(s)(elt));
    if OrbSCCLookup(RhoOrb(l))[i]<>RhoOrbSCCIndex(l) then 
      return fail;
    fi;
    
    f:=RhoOrbMult(RhoOrb(l), RhoOrbSCCIndex(l), i)[2]*elt;
    
    return [i, LambdaPerm(s)(Representative(l), f)];
  end;
  #
  scc:=OrbSCC(RhoOrb(l))[RhoOrbSCCIndex(l)];

  return EnumeratorByEnumerator(l, 
   EnumeratorOfCartesianProduct(scc, SchutzenbergerGroup(l)), 
   convert_out, convert_in, [], record);
end);

#this method is unnecesary if we write a method for RhoOrb of a inverse op
#L-classJDM

InstallMethod(Enumerator, "for L-class of an inverse op acting semigroup",
[IsGreensLClass and IsInverseOpClass and IsActingSemigroupGreensClass],
function(l)
  local record, convert_out, convert_in, scc;

  if HasAsSSortedList(l) then 
    return AsSSortedList(l);
  fi;
  
  record:=rec(parent:=l);
  record.Membership:=function(elm, enum)
    return elm in l;
  end;
  record.PrintObj:=function(enum) 
    Print("<enumerator of L-class>");
  end;
  
  record.Length:=enum-> Size(l);
  #
  convert_out:=function(enum, tuple)
    local l, rep, act;
    if tuple=fail then return fail; fi;
    l:=enum!.parent;
    rep:=Representative(l);
    act:=StabilizerAction(Parent(l));
    return act(LambdaOrbMult(LambdaOrb(l), LambdaOrbSCCIndex(l), tuple[1])[2]
     *rep, tuple[2]);
  end;
  #
  convert_in:=function(enum, elt)
    local l, s, i, f;
    l:=enum!.parent;
    s:=Parent(l); 
   
    if LambdaFunc(s)(elt)<>LambdaFunc(s)(Representative(l)) then 
      return fail;
    fi;
    
    i:=Position(LambdaOrb(l), RhoFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(l))[i]<>LambdaOrbSCCIndex(l) then 
      return fail;
    fi;
    
    f:=LambdaOrbMult(LambdaOrb(l), LambdaOrbSCCIndex(l), i)[1]*elt;
    
    return [i, LambdaPerm(s)(Representative(l), f)];
  end;
  #
  scc:=OrbSCC(LambdaOrb(l))[LambdaOrbSCCIndex(l)];

  return EnumeratorByEnumerator(l, 
   EnumeratorOfCartesianProduct(scc, SchutzenbergerGroup(l)), 
   convert_out, convert_in, [], record);
end);

# same method for regular/inverse

InstallMethod(Enumerator, "for R-class of an acting semigroup",
[IsGreensRClass and IsActingSemigroupGreensClass],
function(r)
  local record, convert_out, convert_in, scc;

  if HasAsSSortedList(r) then 
    return AsSSortedList(r);
  fi;
  
  record:=rec(parent:=r);
  record.Membership:=function(elm, enum)
    return elm in r;
  end;
  record.PrintObj:=function(enum) 
    Print("<enumerator of R-class>");
  end;

  record.Length:=enum-> Size(r);
  #
  convert_out:=function(enum, tuple)
    local r, rep;
    if tuple=fail then return fail; fi;
    r:=enum!.parent;
    rep:=Representative(r);
    return StabilizerAction(Parent(r))(rep,tuple[1])
     *LambdaOrbMult(LambdaOrb(r), LambdaOrbSCCIndex(r), tuple[2])[1];
  end;
  #
  convert_in:=function(enum, elt)
    local r, s, i, f;
    r:=enum!.parent;
    s:=Parent(r); 
   
    if RhoFunc(s)(elt)<>RhoFunc(s)(Representative(r)) then 
      return fail;
    fi;
    
    i:=Position(LambdaOrb(r), LambdaFunc(s)(elt));
    if OrbSCCLookup(LambdaOrb(r))[i]<>LambdaOrbSCCIndex(r) then 
      return fail;
    fi;
    
    f:=elt*LambdaOrbMult(LambdaOrb(r), LambdaOrbSCCIndex(r), i)[2];
    
    return [LambdaPerm(s)(Representative(r), f), i];
  end;
  #
  scc:=OrbSCC(LambdaOrb(r))[LambdaOrbSCCIndex(r)];

  return EnumeratorByEnumerator(r, 
   EnumeratorOfCartesianProduct(SchutzenbergerGroup(r), scc), 
   convert_out, convert_in, [], record);
end);

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
