s:=FullTransformationSemigroup(8);;
f:=Transformation([1,1,2,3,4,5,6,7]);;
ExpandOrbitsOfImages(s);
iter:=IteratorOfGreensDClasses(s);
d:=NextIterator(iter);
d:=NextIterator(iter);
r:=GreensRClassOfElement(s, f);
elts:=Elements(r);;
for i in elts do if not i in d then  fi; od;

# approx. 740ms/690ms with 1000m of memory and profiling off 
# 650ms the first time the for loop is done and 600ms thereafter

rr:=GreensRClassOfElement(s, Transformation([1,1,1,2,3,4,5,6]));
elts:=Elements(rr);;
for i in elts do if not i in d then  fi; od;

# approx. 90ms/55ms with 1000m of memory and profiling off 
# 75ms the first time the for loop is done and 50ms thereafter
# (due to storing RankOfTransformation!)


Sum(List(GreensDClasses(s), Size)); 8^8;

l:=[Transformation( [ 1, 2, 3, 4 ] ), Transformation( [ 1, 3, 2, 3 ] ), 
> Transformation( [ 3, 2, 4, 2 ] ), 
>  Transformation( [ 3, 2, 3, 4 ] ), Transformation( [ 2, 3, 4, 3 ] ), 
> Transformation( [ 1, 3, 1, 3 ] ), 
>  Transformation( [ 4, 2, 4, 2 ] ), Transformation( [ 2, 2, 4, 4 ] ), 
> Transformation( [ 2, 4, 3, 4 ] ), 
>  Transformation( [ 1, 1, 1, 1 ] ) ];;

iter:=IteratorOfDClassReps(s);
for i in [1..15] do NextIterator(iter); od;
iter2:=IteratorOfRClassReps(s);
for i in [1..500] do NextIterator(iter2); od;
NextIterator(iter!.r);

s:=Semigroup(gens);
iter:=IteratorOfDClassReps(s);
for i in [1..15] do NextIterator(iter); od;
NextIterator(iter!.r);

s:=Semigroup(gens); 
iter1:=IteratorOfRClassReps(s);
iter2:=IteratorOfRClassReps(s);
NextIterator(iter1); NextIterator(iter2);

RandomOrderEndo:=function(n)
local out, i, m;

out:=[Random([1..n])];

for i in [1..n-1] do 
	m:=Length(out);
	if out[m]<n then 
		out[m+1]:=Random([out[m]+1..n]);
	else
		out[m+1]:=n;
	fi;
od;

return TransformationNC(out);
end;

s:=OrderPreservingSemigroup(20);
f:=RandomOrderEndo(20);
d:=GreensDClassOfElementNC(s, f);

s:=FullTransformationSemigroup(10);
f:=Transformation( [ 8, 10, 8, 5, 6, 10, 7, 2, 9, 9 ] );
d:=GreensDClassOfElementNC(s, f);; #1s check efficiency here!
Idempotents(d);; #1.3s




gens:=[ Transformation( [ 2, 8, 3, 7, 1, 5, 2, 6 ] ), 
  Transformation( [ 3, 5, 7, 2, 5, 6, 3, 8 ] ), 
  Transformation( [ 4, 1, 8, 3, 5, 7, 3, 5 ] ), 
  Transformation( [ 4, 3, 4, 5, 6, 4, 1, 2 ] ), 
  Transformation( [ 5, 4, 8, 8, 5, 6, 1, 5 ] ), 
  Transformation( [ 6, 7, 4, 1, 4, 1, 6, 2 ] ), 
  Transformation( [ 7, 1, 2, 2, 2, 7, 4, 5 ] ), 
  Transformation( [ 8, 8, 5, 1, 7, 5, 2, 8 ] ) ];;
s:=Semigroup(gens);
f:=Transformation( [ 5, 2, 7, 2, 7, 2, 5, 8 ] );;
d:=GreensDClassOfElementNC(s, f);;
d:=GreensDClassOfElement(s, f);;

GreensRClassReps(d);;
NrGreensRClasses(d);
