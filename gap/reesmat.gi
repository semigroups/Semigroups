############################################################################
##
#W  reesmat.gi
#Y  Copyright (C) 2014                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

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
