gapbind14
=========

`gapbind14` is an (almost) header-only library that exposes C++ types in GAP
and vice versa, mainly to create GAP bindings of existing C++ code. This
project was inspired by (and borrows some code from)
[pybind11](https://pybind11.readthedocs.io/en/latest/).
The aim is the same as that of
[pybind11](https://pybind11.readthedocs.io/en/latest/): to minimize boilerplate
code in GAP extension modules by inferring type information using
compile-time introspection.

This is a work in progress, it probably contains some terrible design decisions
and bugs, and is probably missing many obvious features.

Here's an example of how `gapbind14` can be used (this is actual code used in
the kernel module of the [Semigroups](https://semigroups.github.io/Semigroups)
package for GAP. 

```c++
GAPBIND14_MODULE(libsemigroups, m);

using libsemigroups::Congruence;
using libsemigroups::congruence_type;
using libsemigroups::FroidurePinBase;
using libsemigroups::word_type;

auto congruence_init
    = gapbind14::init<Congruence, congruence_type, FroidurePinBMat const&>;

GAPBIND14_CLASS(m, Congruence);
GAPBIND14_CONSTRUCTOR_OVERLOAD(m, Congruence, create, congruence_init);
GAPBIND14_CLASS_MEM_FN(m, Congruence, nr_generating_pairs, nr_generating_pairs);
GAPBIND14_CLASS_MEM_FN_OVERLOAD(
    m,
    Congruence,
    add_pair,
    add_pair,
    (gapbind14::overload_cast<word_type const&, word_type const&>) );
GAPBIND14_CLASS_MEM_FN(m, Congruence, nr_classes, nr_classes);
GAPBIND14_CLASS_MEM_FN(m, Congruence, word_to_class_index, word_to_class_index);
GAPBIND14_CLASS_MEM_FN(m, Congruence, contains, contains);
GAPBIND14_CLASS_MEM_FN(m, Congruence, less, less);

GAPBIND14_FINALIZE(m);
```

The above code produces a record named `libsemigroups` that's available in GAP:

```gap
gap> libsemigroups;
rec( Congruence := rec( add_pair := function( arg1, arg2, arg3 ) ... end,
      contains := function( arg1, arg2, arg3 ) ... end,
      create := function( arg1 ) ... end,
      less := function( arg1, arg2, arg3 ) ... end,
      nr_classes := function( arg1 ) ... end,
      nr_generating_pairs := function( arg1 ) ... end,
      word_to_class_index := function( arg1, arg2 ) ... end )
```

the component `Congruence` is itself a record whose components are bound to GAP
functions for each one of the member functions declared by `GAPBIND14_CLASS_MEM_FN` or 
`GAPBIND14_CONSTRUCTOR_OVERLOAD`. The following GAP code can then be used to
create C++ object of type ``libsemigroups::Congruence`` and interact with them:

```gap
# where C is a belongs to IsCongruenceByGeneratingPairsRep
S  := Range(C);
CC := libsemigroups.Congruence.create([CongruenceByGeneratingPairsKind(C),
                                       CppFroidurePin(S)]);
add_pair := libsemigroups.Congruence.add_pair;
for pair in CongruenceByGeneratingPairsPairs(C) do
  add_pair(CC, Factorization(S, pair[1]) - 1, Factorization(S, pair[2]) - 1);
od;
return CC;
```
