############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# this file contains methods for every operation/attribute/property that is
# specific to Rees 0-matrix semigroups.

# same method for ideals

InstallMethod(IsomorphismPermGroup,
"for a subsemigroup of a Rees 0-matrix semigroup",
[IsReesZeroMatrixSubsemigroup],
function(S)
  local rep;

  if not IsGroupAsSemigroup(S)  then
   Error( "usage: a subsemigroup of a Rees 0-matrix semigroup satisfying\n", 
    "IsGroupAsSemigroup,");
   return; 
  fi;

  rep:=S.1;
  if rep![1]=0 then # special case for the group consisting of 0
    return MagmaIsomorphismByFunctionsNC(S, Group(()), x-> (), x-> rep);
  fi;

  return MagmaIsomorphismByFunctionsNC(S, 
    Group(List(GeneratorsOfSemigroup(S), x-> x![2])),
      x-> x![2], x-> RMSElement(S, rep![1], x, rep![3]));
end);

# same method for ideals

InstallMethod(GroupOfUnits, "for a Rees 0-matrix subsemigroup",
[IsReesZeroMatrixSubsemigroup],
function(s)
  local r, g, i, j, u;

  if MultiplicativeNeutralElement(s)=fail then
    return fail;
  fi;

  r:=GreensRClassOfElementNC(s, MultiplicativeNeutralElement(s));
  g:=SchutzenbergerGroup(r);
  i:=MultiplicativeNeutralElement(s)![1];
  j:=MultiplicativeNeutralElement(s)![3];

  u:=Semigroup(List(GeneratorsOfGroup(g), x-> RMSElement(s, i, x, j)));
  
  SetIsGroupAsSemigroup(u, true);
  UseIsomorphismRelation(u, g);

  return u;
end);

# this method is better than the generic one for non-exhaustive semigroups

InstallMethod(Random, "for a Rees 0-matrix semigroup", 
[IsReesZeroMatrixSemigroup], 3, # to beat the method for regular non-exhaustive semigroups
function(R)
  return Objectify(TypeReesMatrixSemigroupElements(R), 
   [Random(Rows(R)), Random(UnderlyingSemigroup(R)),
    Random(Columns(R)), Matrix(ParentAttr(R))]);
end);

# this method is just a copy of the library method in GAP 4.7.5 with the extra
# line GeneratorsOfSemigroup, so that the correct (i.e. non-exhaustive) methods
# are used for ReesZeroMatrixSemigroups when the package is loaded. 

InstallMethod(ReesZeroMatrixSemigroup, "for a semigroup and a dense list",
[IsSemigroup, IsDenseList], 
function(S, mat)
  local fam, R, type, x;

  if not ForAll(mat, x-> IsDenseList(x) and Length(x)=Length(mat[1])) then 
    Error("usage: <mat> must be a list of dense lists of equal length,");
    return;
  fi;

  for x in mat do 
    if ForAny(x, s-> not (s=0 or s in S)) then
      Error("usage: the entries of <mat> must be 0 or belong to <S>,");
      return;
    fi;
  od;

  fam := NewFamily( "ReesZeroMatrixSemigroupElementsFamily",
          IsReesZeroMatrixSemigroupElement);

  # create the Rees matrix semigroup
  R := Objectify( NewType( CollectionsFamily( fam ), IsWholeFamily and
   IsReesZeroMatrixSubsemigroup and IsAttributeStoringRep ), rec() );

  # store the type of the elements in the semigroup
  type:=NewType(fam, IsReesZeroMatrixSemigroupElement);
  
  fam!.type:=type;
  SetTypeReesMatrixSemigroupElements(R, type); 
  SetReesMatrixSemigroupOfFamily(fam, R);

  SetMatrix(R, mat);                 SetUnderlyingSemigroup(R, S);
  SetRows(R, [1..Length(mat[1])]);   SetColumns(R, [1..Length(mat)]);
  SetMultiplicativeZero(R, 
   Objectify(TypeReesMatrixSemigroupElements(R), [0]));

  # cannot set IsZeroSimpleSemigroup to be <true> here since the matrix may
  # contain a row or column consisting entirely of 0s!

  if HasIsFinite(S) then 
    SetIsFinite(R, IsFinite(S));
  fi;
  GeneratorsOfSemigroup(R); 
  SetIsSimpleSemigroup(R, false);
  return R;
end);

#

InstallMethod(IsGeneratorsOfInverseSemigroup, 
"for a collection of Rees 0-matrix semigroup elements",
[IsReesZeroMatrixSemigroupElementCollection], ReturnFalse);

#

InstallMethod(ViewString, 
"for a Rees 0-matrix subsemigroup ideal with ideal generators",
[IsReesZeroMatrixSubsemigroup and IsSemigroupIdeal and
HasGeneratorsOfSemigroupIdeal], 
function(I)
  local str, nrgens;
  
  str:="\><";

  if HasIsTrivial(I) and IsTrivial(I) then 
    Append(str, "\>trivial\< ");
  else 
    if HasIsCommutative(I) and IsCommutative(I) then 
      Append(str, "\>commutative\< ");
    fi;
  fi;

  if HasIsTrivial(I) and IsTrivial(I) then 
  elif HasIsZeroSimpleSemigroup(I) and IsZeroSimpleSemigroup(I) then 
    Append(str, "\>0-simple\< ");
  elif HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I) then 
    Append(str, "\>simple\< ");
  fi;

  if HasIsInverseSemigroup(I) and IsInverseSemigroup(I) then 
    Append(str, "\>inverse\< ");
  elif HasIsRegularSemigroup(I) 
   and not (HasIsSimpleSemigroup(I) and IsSimpleSemigroup(I)) then 
    if IsRegularSemigroup(I) then 
      Append(str, "\>regular\< ");
    else
      Append(str, "\>non-regular\< ");
    fi;
  fi;
  
  Append(str, "\>Rees\< \>0-matrix\< \>semigroup\< \>ideal\< ");
  Append(str, "\<with\> ");
  
  nrgens:=Length(GeneratorsOfSemigroupIdeal(I));
  Append(str, ViewString(nrgens));
  Append(str, "\< generator");

  if nrgens>1 or nrgens=0 then 
    Append(str, "s\<");
  else 
    Append(str, "\<");
  fi;
  Append(str, ">\<");

  return str;
end);

#

InstallMethod(MatrixEntries, "for a Rees matrix semigroup",
[IsReesMatrixSemigroup], 
function(R)
  local P;
  return Union(Matrix(R){Columns(R)}{Rows(R)}); 
  # in case R is a proper subsemigroup of another RMS
end);

InstallMethod(MatrixEntries, "for a Rees 0-matrix semigroup",
[IsReesZeroMatrixSemigroup],
function(R)
  return Union(Matrix(R){Columns(R)}{Rows(R)}); 
end);

#

InstallMethod(GreensHClassOfElement, "for a RZMS, pos int, and pos int",
[IsReesZeroMatrixSemigroup, IsPosInt, IsPosInt],
function(R, i, j)
  local rep;
  
  rep:=RMSElement(R, i, Representative(UnderlyingSemigroup(R)), j);
  return GreensHClassOfElement(R, rep);
end);

#

if not IsGrapeAvailable then 
  InstallMethod(RZMSGraph, "for a RZMS", [IsReesZeroMatrixSemigroup],
  function(R)
    Info(InfoWarning, 1, GrapeIsNotAvailableString);
    return fail;
  end);  

else

  InstallMethod(RZMSGraph, "for a RZMS", [IsReesZeroMatrixSemigroup],
  function(R) 
    local mat, n, m, adj;

    mat:=Matrix(R); n:=Length(mat); m:=Length(mat[1]);

    adj:=function(x,y)
      if x<=m and y>m then 
        return not mat[y-m][x]=0;
      elif x>m and y<=m then 
        return not mat[x-m][y]=0;
      else 
        return false;
      fi;
    end;

    return Graph(Group(()), [1..n+m], OnPoints, adj, true);
  end);

fi;

#EOF
