############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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

#EOF
