


gap> a:=SmallSemigroup(6, 15040);
<small semigroup of size 6>
gap> CongruencesSemilatticeByCayleyGraph(a)=CongruencesOfSmallSemigroup(a);
true
gap> enum:=EnumeratorOfSmallSemigroups(5, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 5>
gap> ForAll(enum, x-> CongruencesSemilatticeByCayleyGraph(x)=
> CongruencesOfSmallSemigroup(x));
true
gap> enum:=EnumeratorOfSmallSemigroups(6, IsSemilatticeAsSemigroup, true);
<enumerator of semigroups of size 6>
gap> ForAll(enum, x-> CongruencesSemilatticeByCayleyGraph(x)=
> CongruencesOfSmallSemigroup(x));
true
