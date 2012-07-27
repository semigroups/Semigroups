# A partition of 2n is stored in the following way:
# One plain list l, the first 2n entries say, in which part each of the
# numbers 1..2n lies, l[2*n+1] is the number of parts. Parts are
# numbered from 1 to 2*n+1 and the parts (sets of numbers) are
# numbered in lexicographically ascending order.
#
# Example: If n=3, then the partition [[1,3,4],[2,6],[5]] of [1..6]
#          would be stored as:
#          [1,2,1,1,3,2, 3]
#          since part [1,3,4] is the first part, part [2,6] is the
#          second part, and part [5] is the third part, there are
#          altogether three parts.
#
# The external representation is just a set of sets whose union is
# [1..2*n].

PartitionExtRep := function(p)
  local i,j,n2,q;
  p := Set(p,Set);     # Sort all parts and the outer list
  n2 := Sum(p,Length);
  q := EmptyPlist(n2+1);
  for i in [1..Length(p)] do
    for j in [1..Length(p[i])] do
      q[p[i][j]] := i;
    od;
  od;
  q[n2+1] := Length(p);
  return q;
end;

ExtRepPartition := function(q)
  local i,n2,p;
  n2 := Length(q)-1;
  p := List([1..q[n2+1]],i->[]);
  for i in [1..n2] do
    Add(p[q[i]],i);
  od;
  return p;
end;

PartitionComposition := function(a,b)
  # This composes two partitions of [1..n] in internal rep
  local c,fuse,fuseit,i,n,next,p1,p2,tab3,x,y;
  n := (Length(a)-1)/2;
  Assert(1,n = (Length(b)-1)/2);
  p1 := a[2*n+1];
  p2 := b[2*n+1];
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
      x := fuseit(a[i+n]);
      y := fuseit(b[i]+p1);
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
      x := fuseit(a[i]);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  for i in [n+1..2*n] do
      x := fuseit(b[i]+p1);
      if tab3[x] = 0 then
          tab3[x] := next;
          next := next + 1;
      fi;
      Add(c,tab3[x]);
  od;
  Add(c,next-1);
  return c;
end;
 
Do := function(n)
    local eee,ppp,qqq;
    ppp := PartitionsSet([1..2*n]);
    qqq := List(ppp,PartitionExtRep);
    eee := Filtered(qqq,x->x=PartitionComposition(x,x));
    Print("n=",n," partitions=",Length(ppp)," idempots=",Length(eee),"\n");
    return [n,Length(ppp),Length(eee)];
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
      if x = PartitionComposition(x,x) then
        counteri := counteri + 1; 
      fi; 
    end;
    MakePartitions(n,Tester);
    return [n,counterp,counteri];
end;

