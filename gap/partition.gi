
BindGlobal("BipartitionFamily", NewFamily("BipartitionFamily",
 IsBipartition, CanEasilySortElements, CanEasilySortElements));

BindGlobal("BipartitionType", NewType(BipartitionFamily,
 IsBipartition and IsDataObjectRep and IsActingElt));


# new for 0.7! - \^ - "for a pos int and bipartition" 
############################################################################# 

InstallMethod(\^, "for a pos int and bipartition",
[IsPosInt, IsBipartition], OnPointsBP);

# new for 1.0! - DegreeOfBipartition - "for a bipartition"
############################################################################

InstallMethod(DegreeOfBipartition, "for a bipartition",
[IsBipartition], x-> x[1]);

# new for 1.0! - ELM_LIST - "for a bipartition and pos int"
############################################################################

InstallOtherMethod(ELM_LIST, "for a bipartition and a pos int",
[IsBipartition, IsPosInt], ELM_LIST_BP);

# new for 1.0! - InternalRepOfBipartition - "for a bipartition"
#############################################################################

InternalRepOfBipartition:=f-> List([1..f[1]+2], i-> f[i]);

# new for 1.0! - PrintObj - "for a bipartition"
#############################################################################

InstallMethod(PrintObj, "for a bipartition",
[IsBipartition],
function(f)
  Print("<bipartition: ", ExtRepBipartition(f), " >");
  return;
end);

InstallGlobalFunction(ExtRepBipartition,
function(q)
  local i,n2,p;
  n2 := q[1];
  p := List([1..q[2]],i->[]);
  for i in [1..n2] do
    Add(p[q[i+2]],i);
  od;
  return p;
end);

InstallMethod(\*, "for a bipartition and bipartition",
[IsBipartition, IsBipartition], 
function(a,b)
  # This composes two partitions of [1..n] in internal rep
  local c,fuse,fuseit,i,n,next,p1,p2,tab3,x,y;
  n := a[1]/2;
  Assert(1,n = b[1]/2);
  p1 := a[2];
  p2 := b[2];
  fuse := [1..p1+p2];
  # From now on i in partition a is in part number a[i]
  #         and j in partition b is in part number b[j]+p2
  # The fusion tab always maintains fuse[i] <= i and the fuse function
  # is defined to be 
  #     # We can now easily fuse parts by changing one number in a table.
  fuseit := function(i) 
              while fuse[i] < i do i := fuse[i]; od; 
              return i; 
            end;
  for i in [1..n] do
      # we want to fuse the parts of i+n in a and i in b:
      x := fuseit(a[i+n+2]);
      y := fuseit(b[i+2]+p1);
      if x <> y then
          #Print("Fusing parts ",x," and ",y,"\n");
          if x < y then
              fuse[y] := x;
          else
              fuse[x] := y;
          fi;
      fi;
  od;
  # We can now put together the resulting partition, we take 1..n from a
  # and n+1..2*n and look at the fusion, in which part they are.
  tab3 := 0*[1..p1+p2];    # A table for the old part numbers
  c := EmptyPlist(2*n+1);
  next := 1;
  for i in [1..n] do
      x := fuseit(a[i+2]);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  for i in [n+1..2*n] do
      x := fuseit(b[i+2]+p1);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  #Add(c,next-1);
  return BipartitionByIntRep(Concatenation([2*n, next-1], c));
# return Concatenation([2*n, next-1], c);
end);
 
Do := function(n)
    local eee,ppp,qqq;
    ppp := PartitionsSet([1..2*n]);
    #qqq := List(ppp,PartitionExtRep);
    #eee := Filtered(qqq,x->x=PartitionComposition(x,x));
    #Print("n=",n," partitions=",Length(ppp)," idempots=",Length(eee),"\n");
    #return [n,Length(ppp),Length(eee)];
end;

MakePartitions := function(n,f)
  # This makes all partitions of 2*n in internal format and
  # calls f on them.
  local Recurse,i,v;

  Recurse := function(n,depth,v,nrparts,f)
    local i;
    if depth > 2*n then
        v[2*n+1] := nrparts;
        f(v);
        return;
    fi;
    for i in [1..nrparts] do
        v[depth] := i;
        Recurse(n,depth+1,v,nrparts,f);
    od;
    v[depth] := nrparts+1;
    Recurse(n,depth+1,v,nrparts+1,f);
  end;

  v := EmptyPlist(2*n+1);
  Recurse(n,1,v,0,f);
end;


# Results:

# n=1 partitions=2 idempots=2
# n=2 partitions=15 idempots=12
# n=3 partitions=203 idempots=114
# n=4 partitions=4140 idempots=1512
# n=5 partitions=115975 idempots=25826
# n=6 partitions=4213597 idempots=541254, 33 seconds
# n=7 partitions=190899322 idempots=13479500, 1591 seconds

DoCount := function(n)
    local counteri,counterp,Tester;
    counterp := 0;
    counteri := 0;
    Tester := function(x) 
      counterp := counterp + 1;
      if x = x*x then
        counteri := counteri + 1; 
      fi; 
    end;
    MakePartitions(n,Tester);
    return [n,counterp,counteri];
end;

