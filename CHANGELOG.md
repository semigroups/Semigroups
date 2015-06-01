##CHANGELOG
### Semigroups package for GAP

#### Copyright (C) 2011-15 James D. Mitchell et al.
#### Licensing information can be found in the LICENSE file.

## Version 2.5 (released 01/06/15)

This is a minor release including several bugfixes, lots of minor improvements in the documentation, some improvements in performance, and some new features.

###New Features in Version 2.5

* Semigroups of partial permutations now have a polynomial time
(quadratic in the degree) algorithm for computing the minimal ideal
[[Wilf Wilson](http://wilf.me)]

* A more efficient `IsInverseSemigroup` method for Rees 0-matrix semigroups
is introduced, along with new methods for `Idempotents` and
`NrIdempotents` for inverse Rees 0-matrix semigroups
[[Wilf Wilson](http://wilf.me)]

* The documentation for congruences has been improved and new tests have
been added. [Michael Torpey]

* A `UniversalSemigroupCongruence` now returns a much smaller set
of generating pairs. [Michael Torpey]

###Issue Resolved in Version 2.5

Issue numbers refer to the issues on the [tracker](http://bitbucket.org/james-d-mitchell/semigroups/issues).

*  [Issue 126](https://bitbucket.org/james-d-mitchell/semigroups/issue/126/): testing membership in a Rees 0-matrix semigroup that knows it is inverse sometimes resulted in an error. [[James Mitchell](http://tinyurl.com/jdmitchell)]

*  [Issue 127](https://bitbucket.org/james-d-mitchell/semigroups/issue/127/): the main algorithm for computing with ideals of acting semigroups which know they are regular contained a bug that resulted in incorrect results. In some cases, some \\(\mathscr{D}\\)-classes were counted more than once, and the returned value of `Size` was higher than the actual size of the ideal. [[James Mitchell](http://tinyurl.com/jdmitchell)]
  
*  [Issue 128](https://bitbucket.org/james-d-mitchell/semigroups/issue/128/): in some special cases `UnderlyingSemigroup`, `ViewObj`, `Size`, and related methods, for Rees 0-matrix semigroups over non-groups returned an error. [[James Mitchell](http://tinyurl.com/jdmitchell)]
  
* The universal congruence specified by generating pairs on a (0-)simple
semigroup no longer causes an error. [Michael Torpey]
	
## Version 2.4.1
This is a extremely minor release to change 1 character in the PackageInfo.g
file (wrong package archive URL). 

## Version 2.4 
This is a minor release including several bugfixes, and
improvements in performance, and some new features.

###New Features in Version 2.4

* The function `RepresentativeOfMinimalIdeal` is introduced. [[Wilf
Wilson](http://wilf.me)]

* Transformation semigroups now have a polynomial time (cubic in the
degree) algorithm for computing the minimal ideal [[Wilf Wilson](http://wilf.me)]

* The functions `RectangularBand`, `ZeroSemigroup`, and
`MonogenicSemigroup` are introduced. [[Wilf Wilson](http://wilf.me)]

* A method for choosing a random element of a semigroup has been
introduced in the case that the semigroup knows its set of
elements. This new method choose elements at random with uniform
probability. [[Wilf Wilson](http://wilf.me)]

* The documentation and tests for congruences has been improved.
[Michael Torpey and [Wilf Wilson](http://wilf.me)]

* The functionality for Rees congruences has been rewritten and
improved. [Michael Torpey]

* There is a new `Enumerator` method for congruence classes of a
semigroup congruence. [Michael Torpey]

###Issues Resolved in Version 2.4

Issue numbers refer to the issues on the [tracker](http://bitbucket.org/james-d-mitchell/semigroups/issues).

* [Issue 88](https://bitbucket.org/james-d-mitchell/semigroups/issue/88/): an inefficiency in `JoinIrreducibleDClasses` of an
inverse semigroup ideal resulted in a call to
`GeneratorsOfSemigroup`. 

* [Issue 94](https://bitbucket.org/james-d-mitchell/semigroups/issue/94/): `EquivalenceClasses` of the trivial congruence (generated
by 0 pairs of elements) returned an error. 

* [Issue 95](https://bitbucket.org/james-d-mitchell/semigroups/issue/95/): The class containing the zero element of a Rees
0-matrix semigroup was not returned by `EquivalenceClasses` of a
congruence over a Rees 0-matrix semigroup.

* [Issue 108](https://bitbucket.org/james-d-mitchell/semigroups/issue/108/): `IsRegularSemigroup` with argument a Rees 0-matrix
semigroup returned an error.

* [Issue 119](https://bitbucket.org/james-d-mitchell/semigroups/issue/119/): `NrCongruencesClasses` and related methods did not work for
Rees congruences.

* [Issue 121](https://bitbucket.org/james-d-mitchell/semigroups/issue/121/): `MultiplicativeZero` and `IsMultiplicativeZero` sometimes
returned incorrect results when applied to a non-acting semigroup
(i.e. a semigroup not of transformations, partial permutations,
partitions, or subsemigroups of a Rees 0-matrix semigroup). 

* [Issue 122](https://bitbucket.org/james-d-mitchell/semigroups/issue/122/): A bug in the creation of Green's classes of ideals of semigroups, which resulted in an error.

* [Issue 123](https://bitbucket.org/james-d-mitchell/semigroups/issue/123/): `IsZeroSemigroup` sometimes returned a false positive when applied to a non-acting semigroup. 

## Version 2.3  
This is a minor release including some internal refactoring, and
subsequent bugfixes, and stability improvements.

* [Issue 116](https://bitbucket.org/james-d-mitchell/semigroups/issue/116/) was resolved. In some cases when the default length of
hash tables in Semigroups was set to be very small, a segmentation
fault occurred. This is a bug in the Orb package (see Issue
10), but we worked around it to resolve this issue. 

## Version 2.2
This is a minor release including some bug fixes, performance
improvements, and additional functionality. 

###New Features in Version 2.2

* The functions SmallestElementSemigroup, LargestElementSemigroup, 
  and GeneratorsSmallest.

* Free bands are introduced. 

* Error messages are more uniform.

* The function RegularDClasses was introduced to resolve [Issue 102](https://bitbucket.org/james-d-mitchell/semigroups/issue/102/).

* The documentation and code for semigroup congruences has been
improved, and is better integrated with the core GAP system.

* The functions ReadGenerators and WriteGenerators were improved. 

###Issues Resolved in Version 2.2

* Some minor corrections were made to the methods for creating the
ideals of some semigroups in standard examples semigroups, such as
SingularTransformationSemigroup.

* [Issue 102](https://bitbucket.org/james-d-mitchell/semigroups/issue/102/): we introduced RegularDClasses. 

* [Issue 104](https://bitbucket.org/james-d-mitchell/semigroups/issue/104/): the performance of MaximalSubsemigroups when applied to
an inverse semigroup has been improved. 

* [Issue 105](https://bitbucket.org/james-d-mitchell/semigroups/issue/105/): CyclesOfPartialPerm is now documented and does not
return nonsense. 

* [Issue 106](https://bitbucket.org/james-d-mitchell/semigroups/issue/106/): MaximalSubsemigroups sometimes failed when the
ResClasses package was loaded. We refactored the code so that the
method from ResClasses is no longer applied.

* [Issue 107](https://bitbucket.org/james-d-mitchell/semigroups/issue/107/): A bug in the creation of Green's classes of an ideal of
a semigroup, which sometimes caused an error, has been resolved.
This issue often caused MaximalSubsemigroups to stop in an error.

* [Issue 110](https://bitbucket.org/james-d-mitchell/semigroups/issue/110/): MaximalSubsemigroups can be applied to any class of
semigroup where it is possible to find an isomorphism to a
transformation semigroup. 

* [Issue 111](https://bitbucket.org/james-d-mitchell/semigroups/issue/111/): POPI(1) returned the wrong semigroup. Similar issues
existed in other corner cases, and these have been resolved too.

## Version 2.1.1
This is a very minor release to fix an issue caused by only loading
the packages needed (but not required) by Semigroups.

## Version 2.1
This is a minor release including some bug fixes and performance
improvements. 

###New Features in Version 2.1

* The functions: 
  
  - AsTransformationSemigroup,
  -	AsPartialPermSemigroup, 
  - AsBipartitionSemigroup,
  - AsBlockBijectionSemigroup 

  which are shortcuts to Range(IsomorphismXSemigroup(S)).

* A method for IsTransitive for a transformation semigroup and,
optionally, a positive integer or list of positive integers. This
method is based on Gabow's algorithm for determining the strongly
connected components of a directed graph.

* The functions MeetSemigroupCongruence and JoinSemigroupCongruences
for finding the meet and join of a pair of congruences of a
semigroup.

* There is a new method for IsSynchronizingSemigroup, suggested by
Peter Cameron, with better complexity than the previous method. 

###Issues Resolved in Version 2.1

Issue numbers refer to the issues on the tracker at:

http://bitbucket.org/james-d-mitchell/semigroups/issues

* There was a bug in ReadGeneratorsFile, which meant it sometimes
returned fail. The mode argument for IO_FilteredFile was not given.

* There was a bug in the \in method for a congruence of a Rees
0-matrix semigroup, which sometimes returned the wrong answer for
the zero of the semigroup.

* There was a bug in IteratorFromGeneratorsFile that caused it to
read only every other line in the given file, and to crash if there
were an odd number of lines. 

* There was no hash function for bipartitions.

* The documentation for InverseSubsemigroupByProperty did not
specify the arguments of the function. 

* [Issue 82](https://bitbucket.org/james-d-mitchell/semigroups/issue/82/): it is now possible to take the quotient of a semigroup
by an ideal using the / operator. 

* [Issue 96](https://bitbucket.org/james-d-mitchell/semigroups/issue/96/): IsIsomorphicSemigroup sometimes returned a false
negative by incorrectly comparing the output of
PartialOrderOfDClasses (up to isomorphism) rather than the
transitive reflexive closure of PartialOrderOfDClasses.

* [Issue 97](https://bitbucket.org/james-d-mitchell/semigroups/issue/97/): there was a bug in the Normalizer method, which
caused GAP to crash when the argument was a monoid with 0
generators. 

* [Issue 98](https://bitbucket.org/james-d-mitchell/semigroups/issue/98/): PartitionMonoid(1) returned the wrong answer, it was
missing the non-identity element. 

* [Issue 99](https://bitbucket.org/james-d-mitchell/semigroups/issue/99/): the documentation for PartialOrderOfDClasses was
incorrect. 

* [Issue 103](https://bitbucket.org/james-d-mitchell/semigroups/issue/103/): under certain circumstances an error was given when
trying to compute with an ideal of an inverse semigroup. 

## Version 2.0 
                                    
This is a major release including many new features and several bug
fixes. 

###New Features in Version 2.0

* extensive new features for computing with elements and
subsemigroups of the partition monoid. It is now possible to compute
with semigroups, monoids, inverse semigroups, inverse monoids, and
ideals consisting of elements of the partition monoid. Examples of
subsemigroups of this type are the Brauer monoids, Temperley-Lieb
monoids, and the dual symmetric inverse monoid. See Chapter 5 of the
manual for more details;

* support for ideals of transformation, partial permutation, and
bipartition semigroups, and subsemigroups of Rees 0-matrix
semigroups. It is now possible to compute anything about one of
these ideals that could formerly only be computed about a semigroup
defined by a generating set. Such ideals now use a data structure
similar to that used by semigroups defined by a generating set;

* the new operations IsomorphismSemigroups, IsIsomorphicSemigroup,
and SmallestMultiplicationTable. Some of the methods for this
operation require the Grape package to be fully installed; 

* the new operation MaximalSubsemigroups, which returns the maximal
subsemigroups of an arbitrary semigroup. Some of the methods for
this operation require the Grape package to be fully installed;

* the operation IsMaximalSubsemigroup;

* the new operation Normalizer for computing a subgroup of a
permutation group consisting of those permutations that stabilise,
under conjugation, a transformation, partial perm, or bipartition
semigroup. The genss package is required for this operation in some
cases;

* the new operation CharacterTableOfInverseSemigroup for finding the
character table of an inverse semigroup of partial permutations;

* methods for defining and manipulating the congruences of a Rees
0-matrix semigroup;

* the properties IsCongruenceFreeSemigroup, IsEUnitaryInverseSemigroup;

* the attributes:
  
  * ComponentRepsOfTransformationSemigroup
  * ComponentsOfTransformationSemigroup
  * CyclesOfTransformationSemigroup
  * ComponentRepsOfPartialPermSemigroup 
  * ComponentsOfPartialPermSemigroup
  * CyclesOfPartialPermSemigroup.

* the new function IteratorFromGeneratorsFile that returns an
iterator which reads semigroup elements from a file created using
WriteGenerators. This function is a convenient way of, for example,
looping over a collection of generators in a file without loading
every object in the file into memory. This  might be useful if
the file contains more information than there is available memory;

* the operation EndomorphismsPartition that returns the monoid
of endomorphisms preserving a partition. This monoid is defined
using the minimum possible number of generators;

* a version of the function Splash that attempts to convert a string 
containing a dot or tikz document into a pdf and opens this pdf.
Other file formats are also supported;

* the function DotSemilatticeOfIdempotents that produces a string
containing a dot document of the semilattice of idempotents of an
inverse semigroup grouped by D-class;

* the operation NaturalLeqInverseSemigroup, which is an umbrella
operation for NaturalLeqPartialPerm, and other such functions. 

###Issues Resolved in Version 2.0

Issue numbers refer to the issues on the tracker at:

http://bitbucket.org/james-d-mitchell/semigroups/issues

* the main algorithm underlying many of the methods in Semigroups
has been revised to avoid computing the same information more than
once. Some further internal rearranging and cleaning up was done. 

* MinimalIdeal and SingularTransformationSemigroup now returns an
ideal rather than a semigroup defined by a generating set;

* to reduce the size of the package archive, the examples directory
has been removed. The content of the examples directory is available
on [this webpage](http://tinyurl.com/jdmitchell/data.php).

* several bugs in the setup for subsemigroups of Rees 0-matrix
semigroups were resolved. These issues would have caused GAP to give
an error in certain circumstances.

* [Issue 33](https://bitbucket.org/james-d-mitchell/semigroups/issue/33/): an error was returned when trying to calculate the size,
or multiply elements in the quotient of a semigroup by an ideal.

* [Issue 36](https://bitbucket.org/james-d-mitchell/semigroups/issue/36/), 64: the function SmallGeneratingSet was ambiguous, in
the sense that it was sometimes unclear how to recreate a semigroup
from its small generating set. For example, SmallGeneratingSet of a
monoid could return an empty list, but this empty list could not be
used to recreate the monoid in GAP. This was resolved by introducing
the functions SmallSemigroupGeneratingSet, SmallMonoidGeneratingSet,
SmallInverseSemigroupGeneratingSet, SmallInverseMonoidGeneratingSet.
These functions can also now be applied to collections of elements,
i.e. not only to semigroups. 

* [Issue 47](https://bitbucket.org/james-d-mitchell/semigroups/issue/47/): ClosureSemigroup had several bugs that could, in some
cases, result in incorrect results, or semigroups with invalid data
structures. 

* [Issue 50](https://bitbucket.org/james-d-mitchell/semigroups/issue/50/) and [Issue 59](https://bitbucket.org/james-d-mitchell/semigroups/issue/59/): WriteGenerators wrote nothing to a file in the case
that it was not piping through xz or gzip. 

* [Issue 55](https://bitbucket.org/james-d-mitchell/semigroups/issue/55/): DotDClasses did not work when the argument was a Rees
0-matrix semigroup (it worked as intended when the argument was a
subsemigroup of such a semigroup defined by a generating set). 

* [Issue 56](https://bitbucket.org/james-d-mitchell/semigroups/issue/56/): the functions Monoid and InverseMonoid sometimes
did not contain their identity element.

* [Issue 57](https://bitbucket.org/james-d-mitchell/semigroups/issue/57/): under certain circumstances a bug in the GAP kernel
function INV_KER_TRANS, that didn't handle kernels and
transformations with different length and degree properly, caused
GAP to give an error.

* [Issue 63](https://bitbucket.org/james-d-mitchell/semigroups/issue/63/): there was an error in the GAP library functions Monoid
and InverseMonoid, when they were passed a monoid as an argument. 

* [Issue 63](https://bitbucket.org/james-d-mitchell/semigroups/issue/63/) (and [Issue 4](https://bitbucket.org/james-d-mitchell/semigroups/issue/4/) in the Orb package): a bug in the Orb
package meant that the log of an Orb was not properly updated if the
enumeration stopped early because we found something we were looking
for. This caused Semigroups to return incorrect results in some rare
cases. 

* [Issue 72](https://bitbucket.org/james-d-mitchell/semigroups/issue/72/): the method for IsomorphismTransformationSemigroup
applied to a binary relation monoid returned an isomorphism to a
transformation semigroup which was missing the image of the
identity. 

* [Issue 89](https://bitbucket.org/james-d-mitchell/semigroups/issue/89/): there was a bug in TRANS_IMG_CONJ which failed to handle
transformations of unequal degrees correctly. This causes incorrect
results to be returned when considering semigroups generated by
transformations of unequal degrees.

## Version 1.4 
This is a minor release containing some bug fixes. Specifically:
the functionality of ReadGenerators and WriteGenerators has been
improved to allow the argument to be an IO file object, and support
is added to read and write directly to files compressed using
xz. A minor bug relating to the creation of idempotents in
transformation semigroups which was triggered by the identity
transformation has been resolved. The functions
IsomorphismReesMatrixSemigroup, InjectionPrincipalFactor, and
IsZeroSimpleSemigroup have been revised. IsZeroSimpleSemigroup
formerly returned true for the 2 element zero semigroup, which is
not 0-simple. IsomorphismReesMatrixSemigroup could have returned an
error if called for a semigroup which was not a semigroup of
partial perms or transformations. The use of AsPermutation was
changed to PermutationOfImage where appropriate following a change
to the library methods for AsPermutation. The declarations of
IsomorphismPermGroup and ClosureSemigroup were moved/changed to
avoid warnings that their methods matched more than one
declaration. These warnings were exposed by doing LoadAllPackages,
but were not present when loading Semigroups by itself.

## Version 1.3 
Version 1.3 contains many bug fixes, extensions and improvements in
the documentation, and several new methods and functions. Most
notably are (in no particular order): 

* the methods in Semigroups have been extended to apply to
arbitrary subsemigroups of regular Rees 0-matrix semigroups over
groups; 

* a new method for MaximalSubsemigroups of Rees matrix
semigroup has been implemented; 

* the function Read/WriteSemigroups have been renamed
Read/WriteGenerators and their performance has been improved. It is
now possible to use WriteGenerators to write to a gzipped file; 

* the operation SingularSemigroup has been renamed
SingularTransformationSemigroup;

* the following attributes have been introduced: MinimalDClass,
MaximalDClasses, StructureDescriptionMaximalSubgroups,
StructureDescriptionSchutzenbergerGroups, and IsGreensDLeq.

* the attribute/operation DotDClasses has been introduced. This
allows the D-class diagram of a semigroup to be viewed.

* ComponentRepsOfTransformationSemigroup is reintroduced.

## Version 1.2
This release includes several new methods for inverse semigroups of
partial permutations and for free inverse semigroups. Most notably
among the new methods for inverse semigroups of partial
permutations are:

* SmallerDegreePartialPermRepresentation
* VagnerPrestonRepresentation

for changing the representation of an inverse semigroup of partial 
permutations. The changes in this release were the result of the 
University of St Andrews Research for Undergraduates Summer School 
in 2012, and were largely written by [Wilf Wilson](http://wilf.me) and Robert Hancock.

Free inverse semigroups, and their elements, are also introduced,
this part of the package was written by [Julius
Jonusas](http://www-groups.mcs.st-andrews.ac.uk/~julius/) (who wishes
to acknowledge the support of the Carnegie Trust).

## Version 1.1 
A minor release to fix some technical issues in PackageInfo.g, the
declarations of IsGreens.Class, and a minor change in the output in
one test in everyfunction.tst which was consequence of the
declarations of IsGreens.Class.

## Version 1.0 
The package has been renamed from Citrus to Semigroups. The package
has been completely overhauled, the performance has been improved,
and the code has been generalized so that in the future the same
code can be used to compute with other types of semigroups.  

#Under the name Citrus:

## Version 0.9999 
This is the final release of Citrus (the package will be renamed
Citrus in the next release since the scope of the package has
expanded to include more types of semigroups than just those of
transformations and partial permutations).

A minor release fixing several bugs relating to inverse semigroups
of partial permutations pointed out by partcipants at the
University of St Andrews Research for Undergraduates Summer School
in July 2012.  Most notably by Demi Allen, Casey Donoven, Rhiannon
Dougall, Robert Hancock, and [Wilf Wilson](http://wilf.me). More specifically,
SymmetricInverseSemigroup(n) returned an incorrect answer when n=1
or n=2, \in for the empty mapping and an inverse semigroup of
partial perms sometimes incorrectly returned false, some harmless
compiler warnings were displayed when using more recent versions of
gcc, NaturalLeqPP sometimes returned the incorrect value, there was
no method for IsInverseSemigroup or IsInverseMonoid for a semigroup
of partial perms.

## Version 0.999
A minor release fixing several bugs relating to partial
permutations and monoids thereof, pointed out by Jack Schmidt.
More specifically, MultiplicativeZero sometimes incorrectly
returned fail for a inverse semigroup of partial permutations,
sometimes PartialPerm incorrectly returned fail when given a dense
range as an argument, sometimes the size of an inverse monoid was 1
more than the correct value, and RestrictedPP sometimes failed when
it should not have. 

## Version 0.99
another minor release. Specific changes were: removed the
declaration of SmallGeneratingSet for IsSemigroup since it appears
not to be used and caused a warning to be shown when rcwa was
loaded after Citrus. Added a new abstract to the PackageInfo.g
file, and the documentation, and updated the webpages, in
particular so that the html version of the manual is linked to that
on the GAP webpage and the links to other manuals work. 

## Version 0.9
renamed the function for creating the semigroup of order-preserving
transformations on an n-chain from O to OrderEndomorphisms after it
was pointed out that it is not sensible to have function names with
one character. Also made some minor adjustments to the manual.

## Version 0.8 
minor changes due to incompatibility with Smallsemi 0.6.4 which 
caused some test files to fail when Nilmat was loaded after
Citrus.  The clashes and the failed test were caused by various
properties being declared for IsTransformationSemigroup rather than
IsSemigroup. 

## Version 0.7
the most major change is the introduction of special methods for
partial permutations and inverse semigroups. So that these methods
are efficient, a GAP kernel component (in C) has also been
introduced for various low-level computations with partial
permutations. Essentially all functions previously available for
transformation semigroups are now available for inverse semigroups
of partial permutations. The manual has been expanded and
reorganised, some standard examples have been included (semigroups
of order preserving transformations or partial permutations, the
symmetric inverse semigroup, the full matrix semigroup over a
finite field), the endomorphism monoids of the non-abelian groups
with at most 64 elements have been included in the catalogues of
examples, the functions InjectionPrincipalFactor,
IsomorphismReesMatrixSemigroup, and PrincipalFactor, and some
specific properties and attributes of inverse semigroups have been
introduced (such as IsFactorisableSemigroup and
PrimitiveIdempotents).

## Version 0.6 
fixed a bug relating to the creation of transformation semigroups 
using MagmaByGenerators. Also added the global variable 
CitrusOptionsRec containing the default values of the options
used by Citrus when creating a semigroup. 

## Version 0.5 
major changes are: the documentation has been further revised,
functions for creating semigroups and monoids with certain
options have been introduced, several functions have had the word
`Greens' removed from their names to reduce the length, the
operation ClosureSemigroup has been introduced, the functions
ReadCitrus and WriteCitrus for reading and writing
transformations to a file have been introduced, several catalogues
of examples of transformation semigroups are now included in the
examples directory, methods for creating a Green's class inside
another Green's class have been included (such as an R-class of a
D-class or an H-class of an L-class), the hash functions used for
transformations etc have been improved. 

Some minor bugs have been fixed, and new methods or functions with
the following names have also been introduced:

AntiIsomorphismTransformationSemigroup (for a trans. semigroup),
IdempotentGeneratedSubsemigp, IsomorphismTransformationSemigroup
(for a perm. gp), IsomorphismTransformationMonoid (for a perm. gp),
NrElementsOfRank.

## Version 0.4 
major changes are: the documentation has been updated, some changes
to core functions for R-classes/image orbits have resulted in a 
performance improvement, there is a method for the operation
Factorization allowing an arbitrary element of a transformation
semigroup to be expressed as a product of the generators. 

Some minor bugs have been fixed, and new methods or functions with
the following names have also been introduced:

OrbSCC, OrbSCCLookup, OrbSCCTruthTable, 
ReverseSchreierTreeOfSCC, SchreierTreeOfSCC,
IsomorphismTransformationSemigroup (for a perm. gp). 

## Version 0.3 
fixed a critical (but rare) bug in AddToOrbitsOfKernels that caused
computations relating to D-classes, H-classes, or L-classes to
return incorrect answers in some cases. 

## Version 0.2 
updated the method for \^ for a transformation and perm so that it
is more efficient than the library method, same for \* for a perm 
and transformation. New method for StructureDescription of a
Brandt semigroup, and IsSubset for a trans. semigroup and trans.
coll. 

fixed bugs in IndexPeriodOfTransformation (it returned incorrect
results) and AsPermutation. Also reduce hash table lengths so that 
Citrus uses less memory. Fixed bug that triggered an infinite
loop when trying to find elements of a trivial trans. semigroup.  

added the functions CitrusDefaultMem, CitrusHiMem,
CitrusLoMem, CitrusVeryLoMem, IsBrandtSemigroup,
IsLeftSimple, IsMonogenicSemigroup, IsRightSimple,
IsZeroRectangularBand, IsZeroSimpleSemigroup. 

