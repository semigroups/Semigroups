# CHANGELOG – [Semigroups package][] for [GAP][]
Copyright © 2011-2025 [James D. Mitchell][] et al.

Licensing information can be found in the `LICENSE` file.

## Version 5.5.3 (released 17/07/2025)

This is a minor release including some changes for compatibility with GAP by
@fingolfin.

## Version 5.5.2 (released 11/07/2025)

This is a minor release removing some extraneous files included in the release
archive for v5.5.1, and updating a test file due to some changes in a recent
version of GAP.

## Version 5.5.1 (released 19/06/2025)

This release containing one fix:

* Fix `TikzString` for bipartitions by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/1060

and updating the version of ``libsemigroups`` required to version 2.7.4 which
resolves:

https://github.com/semigroups/Semigroups/issues/1064


## Version 5.5.0 (released 19/02/2025)

## What's Changed

* Fix bug in gen pairs by @james-d-mitchell in
https://github.com/semigroups/Semigroups/pull/1043
* Generalize `IsSelfDualSemigroup` declaration by @fingolfin in
https://github.com/semigroups/Semigroups/pull/1044
* Better inverses by @Tianrun-Y in
https://github.com/semigroups/Semigroups/pull/1021
* Use GAP kernel helper `IS_STRING_REP` to test for strings by @fingolfin in
https://github.com/semigroups/Semigroups/pull/1045
* Better Nambooripad by @Tianrun-Y in
https://github.com/semigroups/Semigroups/pull/1026
* Fix spelling by @james-d-mitchell in
https://github.com/semigroups/Semigroups/pull/1050
* Avoid obsolete `C_NEW_STRING` macro by @fingolfin in
https://github.com/semigroups/Semigroups/pull/1051

**Full Changelog**: https://github.com/semigroups/Semigroups/compare/v5.4.0...v5.5.0

## Version 5.4.0 (released 19/10/2024)

## What's Changed

* Improved IsIsomorphicSemigroup Method by @awesometillman in https://github.com/semigroups/Semigroups/pull/1023
* fix typo in nambooripad partial order documentation by @Tianrun-Y in https://github.com/semigroups/Semigroups/pull/1030
* Better kernel module checking by @Joseph-Edwards in https://github.com/semigroups/Semigroups/pull/1032
* Fix ChooseHashFunction rank issue. by @reiniscirpons in https://github.com/semigroups/Semigroups/pull/1035
* kernel: Semigroups can be used in GAP on julia by @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/1036
* Use LoadKernelExtension for gapbind_demo by @fingolfin in https://github.com/semigroups/Semigroups/pull/1039

## New Contributors
* @awesometillman made their first contribution in https://github.com/semigroups/Semigroups/pull/1023
* @Tianrun-Y made their first contribution in https://github.com/semigroups/Semigroups/pull/1030

**Full Changelog**: https://github.com/semigroups/Semigroups/compare/v5.3.7...v5.4.0

## Version 5.3.7 (released 11/03/2024)

This is a minor release with some changes for compatibility with GAP.

* Update Joe's info by @Joseph-Edwards in
  https://github.com/semigroups/Semigroups/pull/1002
* Workaround for GAP 4.12 test failure due to new GAPDoc version by @fingolfin
  in https://github.com/semigroups/Semigroups/pull/1004
* Use MarkAllButFirstSubBags for T_BIPART by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/1003

## Version 5.3.6 (released 19/02/2024)

This is a minor release with some changes related to forthcoming changes in
GAP. These changes were implemented by Fabian Zickgraf.

### Version 5.3.5 (released 14/02/2024)

This is a minor release fixing an issue in the kernel extension (reported by
Michael Orlitzky in Issue #996), with some further changes for compatibility
with GAP (by Max Horn), and fixing a bug in `IrredundantGeneratingSubset` which
gave an error for monogenic semigroups (by Reinis Cirpons).

### Version 5.3.4 (released 24/01/2024)

This is a minor release that fixes some issue in the build system, applies some
new linting rules, and perhaps most importantly re-enables the use of [HPCombi][]
in [libsemigroups][]. A number of different things were wrong which meant that:

* the correct configuration flags weren't set for building [Semigroups][] when
  [HPCombi][] is enabled;
* [HPCombi][] was not detected properly because of a mistake in the path;
* the kernel module couldn't be built even when the last two items were
  resolved, due to some missing functionality for [HPCombi][] types.

These issues arose because the main author no longer has access to any machines
running x86, and so the support for [HPCombi][] slowly eroded, unnoticed by me,
over time.

The other changes in this release are:

* some changes of PreImages... to NC versions (for compatibility with
  forthcoming changes to GAP) by @cdwensley in
  https://github.com/semigroups/Semigroups/pull/965
* buildsys: unify rpath handling by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/976
* Support GAP on Cygwin with libtool removed (for compatibility with
  forthcoming changes to GAP) by @ChrisJefferson in
  https://github.com/semigroups/Semigroups/pull/979
* Fix for [HPCombi][] by @james-d-mitchell (fixes the issues itemised above) in
  https://github.com/semigroups/Semigroups/pull/978
* Linting by @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/980

### New Contributors
* @cdwensley made their first contribution in
  https://github.com/semigroups/Semigroups/pull/965

**Full Changelog**: https://github.com/semigroups/Semigroups/compare/v5.3.3...v5.3.4

### Version 5.3.3 (released 05/01/2024)

This is a minor release bumping the included+required versions of libsemigroups
to the latest version 2.7.2.

### Version 5.3.2 (released 07/10/2023)

This is a minor release with some bugfixes and minor improvements. Support
for GAP workspaces was broken in v5.3.0, and this issue has been resolved in
v5.3.2.

### Version 5.3.1 (released 19/09/2023)

This is a minor release fixing a minor issue in the build system.

### Version 5.3.0 (released 18/09/2023)

This release contains a fairly large number of changes, fixes, and
improvements. The biggest changes/new features in this release are:

* The build system has been completely updated by @fingolfin and @dimapase:
  - Convert buildsystem to use Makefile.gappkg instead of automake by @fingolfin
    in https://github.com/semigroups/Semigroups/pull/902
  - Use the C++ compiler to link semigroups.so by @fingolfin in
    https://github.com/semigroups/Semigroups/pull/942
  - Work around autoconf 2.69 bugs by @fingolfin in
    https://github.com/semigroups/Semigroups/pull/947
  - set rpath for external libsemigroups by @dimpase in
    https://github.com/semigroups/Semigroups/pull/954
  - buildsystem: sane warning if not using gmake by @james-d-mitchell in
    https://github.com/semigroups/Semigroups/pull/955
* Some updates to improve the performance of `LatticeOfCongruences` and friends
  by @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/884
* Add `GreensLeftMultipliers` and related by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/925

The following bugs were fixed and/or issues resolved:

* Use same URL as in my other packages by @olexandr-konovalov in
  https://github.com/semigroups/Semigroups/pull/907
* Fix a few typos inn the doc by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/913
* Increase required version of Digraphs and remove some redundant code by
  @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/921
* greens: fix `HClassReps(LClass)` for acting semigroups by @james-d-mitchell
  in https://github.com/semigroups/Semigroups/pull/927
* greens: fix bug in `RightGreensMultiplier` for acting semigroups by
  @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/930
* congsemigraph: add `\=` method by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/933
* greens: fix `RhoOrbStabChain(DClass)` by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/932
* doc: remove empty xml files by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/934
* Fix a typo that breaks HPCombi support by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/941
* typo (ERNEL<-KERNEL) in configure.ac by @dimpase in
  https://github.com/semigroups/Semigroups/pull/944
* fix the URL for libsemigroups in README.md by @dimpase in
  https://github.com/semigroups/Semigroups/pull/945
* kernel: fix mem leaks by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/951

### Version 5.2.1 (released 05/03/2023)

This is a minor release with some changes for compatibility with upcoming
changes to GAP, and some updates to the build system.

### Version 5.2.0 (released 01/12/2022)

This release contains a number of fixes and improvements, and one new feature,
which is a method for `IsSelfDualSemigroup` and related functionality for
semigroups satisfying `CanUseFroidurePin`:

* m4/find_gap.m4: update by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/887
* Adds method for `IsSelfDualSemigroup` and related functionality by
  @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/890
* Fix some bugs by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/889
* Fix closure semigroup for non-libsemigroups types by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/891
* Add missing tests and doc for `IsSelfDualSemigroup` and friends by
  @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/892
* Change `-march=avx` -> `-mavx` by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/896
* Fix faulty usage of 0-simple RZMS translations code by @flsmith in
  https://github.com/semigroups/Semigroups/pull/893
* build: replace `m4/ax_check_hpcombi` by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/897

### Version 5.1.0 (released 31/10/2022)

The following major new features have been added in this version:

* extensive support for translations by @flsmith in
  https://github.com/semigroups/Semigroups/pull/720.
  See Chapter 18 of the manual for details.
* support for `Sims1` in libsemigroups by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/873. This manifests in the new
  functions:
  * `IteratorOfRightCongruences`, `IteratorOfLeftCongruences`
  * `NumberOfRightCongruences`, `NumberOfLeftCongruences`
  * `SmalerDegreeTransformationRepresentation`

The following bugs were resolved:

* `IsDualSemigroupRep` is no longer a representation by @flsmith in
  https://github.com/semigroups/Semigroups/pull/863
* fix an issue in `SemigroupIsomorphismByImages` by @james-d-mitchell
  in https://github.com/semigroups/Semigroups/pull/866
* set `IsInjective` flag before calling `SetNiceMonomorphism` by @ThomasBreuer
  in https://github.com/semigroups/Semigroups/pull/870
* Fix issue 869 (the multiplication table of fp semigroups/monoids was
  sometimes incorrect) by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/871
* Resolve Issue #868 (there was recursion depth trap error when trying to
  create an fp monoid from a small semigroup)by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/872
* semifp: add missing methods for fp monoids/semigroups by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/874
* Fix the README to use the correct version of GAP that's required by
  @james-d-mitchell in https://github.com/semigroups/Semigroups/pull/876
* Replace gap implementation of `EqualInFreeBand` with C++ implementation from
  libsemigroups by @reiniscirpons in
  https://github.com/semigroups/Semigroups/pull/877
* Remove an obsolete comment by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/878
* Fix extreme tests by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/880

## New Contributors
* @ThomasBreuer made their first contribution in https://github.com/semigroups/Semigroups/pull/870
* @reiniscirpons made their first contribution in https://github.com/semigroups/Semigroups/pull/877

### Version 5.0.2 (released 12/08/2022)

This is a minor release hopefully resolving some crashes originating in
[libsemigroups][].

### Version 5.0.1 (released 11/08/2022)

This is a minor release resolving some issues in the kernel module and build
system [[Max Horn][]].

### Version 5.0.0 (released 05/08/2022)

This is a major release with several new features and some backwards
incompatible changes prompted by changes in GAP 4.12. Note that to use version
5.0.0 of [Semigroups][] GAP 4.12 is required. At the time of writing GAP 4.12
has not yet been released, and so the development version of GAP (in the
`master` branch of the git repo hosted on github) is required for
[Semigroups][] version 5.0.0.

The backwards incompatible changes in this release related to matrices over the
integers or over a finite field. In previous versions of [Semigroups][] there
was an implementation of matrices over the integers and over finite fields,
because at the time they were written it was not possible to use the matrices
in the GAP library. This is no longer the case, and some changes in the GAP
library for version 4.12, meant that the implementation in [Semigroups][] had
to be removed.

Previously, to create a matrix over the integers you could do:

    Matrix(IsIntegerMatrix, [[0, 1], [1, 0]]);

The equivalent in version 5.0.0 of [Semigroups][] is:

    Matrix(Integers, [[0, 1], [1, 0]]);

where `Integers` is the ring of integers. The changes for matrices over finite
fields are mostly internal, and it was, and still is, possible to create such
matrices using, for example,

    Matrix(GF(4), Z(4) * [[0, 1], [1, 0]]);

In versions of [Semigroups][] before 5.0.0, the filter
`IsMatrixOverFiniteField` could also be used when constructing matrices, and
these features have been removed in version 5.0.0.
See: https://github.com/semigroups/Semigroups/pull/827

The new features introduced in version 5.0.0 are:

* Add support for partial orders of L/R-classes by [Wilf A. Wilson][] in
  https://github.com/semigroups/Semigroups/pull/415
* Support for homomorphisms was introduced by Artemis Konstantinidi,
  Chinmay Nagpal, and [James D. Mitchell][] in
  https://github.com/semigroups/Semigroups/pull/797
  and
  https://github.com/semigroups/Semigroups/pull/828

### Version 4.0.3 (released 01/07/2022)

This is a minor release that includes a number of improvements and bug fixes:

* Update authors + mailmap by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/837
* Resolve issue #835 (implement `MultiplicationTable` for fp semigroups and
  monoids) by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/838
* Improve ccache hit rate in CI by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/840
* Improve support for quotient semigroups by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/841
* semigrp: remove 2 unnec. methods after #841 by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/843
* congsemigraph: add meet method by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/842
* Remove superfluous strings from error messages by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/845

### Version 4.0.2 (released 05/06/2022)

This is a minor release that includes a number of minor fixes:

* conglatt: fix ordering issue by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/829
* Add support for partial orders of L/R-classes by @wilfwilson in
  https://github.com/semigroups/Semigroups/pull/415
* conglatt: avoid recomputing principal congs by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/832
* semipperm: allow SmallerDegree for other types by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/833
* semipperm: resolve issue 817 by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/834

### Version 4.0.1 (released 21/04/2022)

This is a minor release that includes a number of changes:

* Avoid using `SIZEOF_VOID_P` by @wilfwilson in
  https://github.com/semigroups/Semigroups/pull/803
* doc: allow manual compile without all deps by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/810
* Bump libsemigroups -> v2.1.4 by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/809
* Change http -> https by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/818
* Remove spurious dependency on libgmp by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/820
* Improve compatibility with GAP's MatrixObj project by @fingolfin in
  https://github.com/semigroups/Semigroups/pull/819
* Better congruence lattices by @james-d-mitchell in
  https://github.com/semigroups/Semigroups/pull/815
* Add more matrix generators by @flsmith in
  https://github.com/semigroups/Semigroups/pull/822

### Version 4.0.0 (released 28/02/2022)

This is a major release which simplifies and improves many aspects of the
package. The kernel module has been simplified significantly, to make it easier
to incorporate future changes in  [libsemigroups][], and some superfluous
functionality was withdrawn.

### Version 3.4.2 (released 07/02/2021)

This is a minor release resolving some issues in v3.4.1  [[James D. Mitchell][]],
with a number of improvements by [Wilf A. Wilson][].

### Version 3.4.1 (released 28/01/2021)

This is a minor release including some bugfixes and improvements. The required
minimum version of [libsemigroups][] is increased from v1.1.0 to v1.3.2. The
required version of `Digraphs` is increased to v1.2.0. The `datastructures`
package was implicitly required (via `Digraphs`), and now at least version
v0.2.5 is required explicitly. The version of GAP required was implicitly at
least v4.10.0, but was explicitly v4.9.0, it is now explicitly v4.10.0.

### Version 3.4.0 (released 28/08/2020)

This is a minor release with the following new features added to the package:

* `CanonicalReesMatrixSemigroup` and `CanonicalReesZeroMatrixSemigroup` written by Chris Russell.
* `ParseRelations` for finitely presented semigroups or monoids written by [Luke Elliott][].
* `TikzString` method was implemented for collections of PBRs by [Finn Smith][].

The following bugs/issues were resolved:

* [Issue #685](https://github.com/gap-packages/Semigroups/issues/685): after some somewhat recent changes in [libsemigroups][] some infinite finitely presented semigroups in GAP reported their size as being `-2`. Fixed by [James D. Mitchell][].
* [Issue #680](https://github.com/gap-packages/Semigroups/issues/680): Semigroups did not compile with recent git versions of [libsemigroups][]. Reported by [Wilf A. Wilson][] fixed by [James D. Mitchell][].

The required minimum version of [libsemigroups][] is now v1.1.0,

### Version 3.3.2 (released 28/06/2020)

This is a very minor release fixing the following issues.

* [Issue #676](https://github.com/gap-packages/Semigroups/issues/676) and
  [Issue #677](https://github.com/gap-packages/Semigroups/issues/677) reported
  by Reinis Cirpons, where for some congruences `NonTrivialCongruenceClasses`
  failed to work, but `CongruenceClasses` did work. This was caused by an issue
  in `EquivalenceRelationPartition`; resolved by [James D. Mitchell][].

* An issue in the equality, less than, and product methods for PBRs which
  previously permitted PBRs of different degrees to be given as arguments,
  resulting in unhelpful errors from within the implementations of these
  methods; resolved by [James D. Mitchell][].

### Version 3.3.1 (released 28/05/2020)

This is a minor release fixing some issues in the build system [Max Horn][]
related to compatibility with future versions of GAP, and some other minor
fixes by [James D. Mitchell][] and [Wilf A. Wilson][].

### Version 3.3.0 (released 28/04/2020)

This is a minor release fixing some issues in the build system
[Chris Jefferson][], some improvements to the isomorphism capabilities of the
package [James D. Mitchell][], and the introduction of a rudimentary function
to compute the automorphism group of an arbitrary semigroup. From v3.3.0, the
Semigroups package requires the [images package][] version 1.3.0 or higher.

### Version 3.2.5 (released 28/03/2020)

This is a minor release to fixing some build issues for windows [Chris Jefferson][]
and fixing an issue in `IsRowTrimBooleanMat`, `IsColTrimBooleanMat`, and
`IsTrimBooleanMat` [Finn Smith][] see
[Issue #654](https://github.com/gap-packages/Semigroups/issues/654) for more details.

### Version 3.2.4 (released 28/02/2020)

This is a minor release to fix a memory leak and some other issues in the
kernel module.

### Version 3.2.3 (released 07/02/2020)

This is a minor release to make some changes for compatibility with the
[Digraphs package][] v1.1.1 and [GAP][], and to fix a memory leak in the kernel
module.

### Version 3.2.2 (released 17/01/2020)

This is a minor release to update the version of [libsemigroups][] to v1.0.5,
to include some changes for compatibility with [GAP][], and to fix the following issues

* [Issue #632](https://github.com/gap-packages/Semigroups/issues/632) where
  file objects were not closed properly if an error was given;
* [Issue #636](https://github.com/gap-packages/Semigroups/issues/636) the
  compiler flag `-march=native` is only added if it is supported by the compiler;
* [Issue #637](https://github.com/gap-packages/Semigroups/issues/637) where
  `IsomorphismPermGroup` sometimes returned an incorrect answer.

The changes in this release were made by [Max Horn][] and [James D. Mitchell][].

### Version 3.2.1 (released 04/12/2019)

This is a minor release to update the version of [libsemigroups][] to v1.0.2,
and to include some changes for compatibility with [GAP][]. The changes in this
release were made by [James D. Mitchell][] and [Wilf A.  Wilson][].

### Version 3.2.0 (released 04/10/2019)

This is a minor release including several cleanups, changes for compatibility
with [GAP][], and to update the required version of the [Digraphs package][] to
v1.0.0. The changes in this release were made by [James D. Mitchell][] and
[Wilf A.  Wilson][].

### Version 3.1.5 (released 19/09/2019)

This is a minor release including several cleanups, changes for compatibility
with [GAP][], and to add the possibility to pass a `RandomSource` as a first
argument to some methods. All the changes in this release were made by
[Max Horn][].

### Version 3.1.4 (released 30/08/2019)

This is a minor release fixing some minor issues, mostly related to
compatibility with [GAP][] 4.11.

### Version 3.1.3 (released 07/06/2019)

This is a minor release of the Semigroups package, including some improvements
to `SmallestIdempotentPower` and `RepresentativeOfMinimalIdeal` by
[Wilf A.  Wilson][] and some other minor modifications.  Semigroups now requires
version 4.8.2 of the [orb package][], or newer.

### Version 3.1.2 (released 15/03/2019)

This is a minor release of the Semigroups package, including some changes to the
build system (by [D. V. Pasechnik][]), one issue was fixed
([Issue 581](https://github.com/gap-packages/Semigroups/issues/581) by Chris
Russell and [James D. Mitchell][]), and the version of [libsemigroups][]
required was increased to 0.6.7.

### Version 3.1.1 (released 15/02/2019)

This is a minor release of the Semigroups package, including some minor changes
in the kernel module (by [Max Horn][]) and the bibliography section of the
manual has been fixed and updated (by Murray Whyte).

### Version 3.1.0 (released 14/01/2019)

This is a major release of the Semigroups package, including several changes to
existing behaviour, new functionality, and bugfixes.

#### Changed functionality in this release

* Congruence posets are now digraph objects (in the sense of the
  [Digraphs package][]) rather than lists of lists. This applies to
  `LatticeOfCongruences`, `PosetOfCongruences`, and several other functions.
  This change was made by [Michael Young][] in
  [PR #385](https://github.com/gap-packages/Semigroups/pull/385).
* The attributes `LeftCayleyDigraph` and `RightCayleyDigraph` are introduced to
  replace `LeftCayleyGraphSemigroup` and `RightCayleyGraphSemigroup`. These new
  attributes return digraph objects (in the sense of the [Digraphs package][])
  rather than lists of out-neighbours. There are also several new functions for
  visualising Cayley digraphs, such as `TikzLeftCayleyDigraph`,
  `DotLeftCayleyDigraph`, and `DotStringDigraph`.  This functionality was all
  added by [James D. Mitchell][] in
  [PR #348](https://github.com/gap-packages/Semigroups/pull/348).
* `IteratorFromPickledFile` has been replaced by `IteratorFromGeneratorsFile`,
  and `IteratorFromOldGeneratorsFile` has been removed. The behaviour of
  `WriteGenerators` has also been modified slightly.  This work was done by
  [James D. Mitchell][] in
  [PR #419](https://github.com/gap-packages/Semigroups/pull/419).

#### New functionality in this release

* Finn Smith added support for creating and representing dual semigroups in
  [PR #470](https://github.com/gap-packages/Semigroups/pull/470),
  via the new commands `DualSemigroup` and `AntiIsomorphismDualSemigroup`.
* Chris Russell added support for representing E-unitary inverse semigroups by
  McAlister triple semigroups, via the command `McAlisterTripleSemigroup` and
  others, in the PRs
  [#271](https://github.com/gap-packages/Semigroups/pull/271) and
  [#507](https://github.com/gap-packages/Semigroups/pull/507). Chris also
  introduced the related functions `EUnitaryInverseCover`,
  `IsFInverseSemigroup`, and `IsFInverseMonoid`.
* It is now possible to use the Semigroups package to construct direct products
  of arbitrary finite semigroups with the command `DirectProduct`. Direct
  products created in this way support the commands `Projection` and
  `Embedding`. To enable the creation of direct products of arbitrary finite
  semigroups, the related commands `NonTrivialFactorization`,
  `IndecomposableElements`, and `IsSurjectiveSemigroup` were introduced. This
  work was done by [Wilf A. Wilson][] in
  PRs [#347](https://github.com/gap-packages/Semigroups/pull/347)
  and [#558](https://github.com/gap-packages/Semigroups/pull/558).
* The functions `NambooripadPartialOrder` and `NambooripadLeqRegularSemigroup`
  were added by Chris Russell in PRs
  [#467](https://github.com/gap-packages/Semigroups/pull/467) and
  [#479](https://github.com/gap-packages/Semigroups/pull/479).
* The option `numbers` is available when calling `DotString` on a congruence
  poset (added by [Michael Young][] in
  [PR #455](https://github.com/gap-packages/Semigroups/pull/455)).
* More `Info` statements can be produced, if desired, during computations of
  congruence lattices (added by [Michael Young][] in
  [PR #403](https://github.com/gap-packages/Semigroups/pull/403)).
* It is now possible to construct all non-empty two-sided ideals of a finite
  semigroup with the command `Ideals` (added by [Michael Young][] in
  [PR #500](https://github.com/gap-packages/Semigroups/pull/500)).
* `WriteMultiplicatiomTable`, `ReadMultiplicationTable`, and
  `IteratorFromMultiplicationTableFile` were added by Chris Russell in
  [PR #339](https://github.com/gap-packages/Digraphs/pull/339)
* A new constructor `BrandtSemigroup` was added by [James D. Mitchell][] in
  [PR #411](https://github.com/gap-packages/Semigroups/pull/411).
* The command `WreathProduct` can be now applied to construct the wreath
  producth of a transformation monoid or permutation group with a transformation
  semigroup or permutation group. This work was done by Fernando Flores Brito in
  [PR #262](https://github.com/gap-packages/Semigroups/pull/262).
* `SemigroupsTestAll` was introduced by [Michael Young][] in
  PRs [#404](https://github.com/gap-packages/Semigroups/pull/404)
  and [#446](https://github.com/gap-packages/Semigroups/pull/446).
* Some basic methods for `MinimalSemigroupGeneratingSet` were implemented by
  [Wilf A. Wilson][] in
  [PR #407](https://github.com/gap-packages/Semigroups/pull/407).
* It is now possible to write and read Rees matrix semigroups and Rees 0-matrix
  semigroups to and from files with `WriteGenerators` and `ReadGenerators`
  (added by [James D. Mitchell][] in
  [PR #569](https://github.com/gap-packages/Semigroups/pull/569)).
* Special methods for `IsIdempotentGenerated` and
  `IdempotentGeneratedSubsemigroup` for Rees matrix semigroups and Rees 0-matrix
  semigroups were added by [Wilf A. Wilson][] in
  [PR #325](https://github.com/gap-packages/Semigroups/pull/325).
* Special methods for `IsomorphismPartialPermSemigroup` for groups and
  zero-groups were added by [Wilf A. Wilson][] in
  [PR #406](https://github.com/gap-packages/Semigroups/pull/406).
* Semigroups in more kinds of families are now comparable via `\<`, including
  pairs of semigroups of partial permutations, pairs of semigroups of
  bipartitions, and pairs of semigroups of PBRs (added by
  [James D. Mitchell][] in
  [PR #568](https://github.com/gap-packages/Semigroups/pull/568)).

#### Issues resolved by this release

* The documentation now clarifies that only those subsemigroups of regular Rees
  0-matrix semigroups defined over *permutation* groups are acting semigroups.
  This problem was reported by [Wilf A. Wilson][] in
  [Issue #336](https://github.com/gap-packages/Semigroups/issues/336) and fixed
  by [James D. Mitchell][] in
  [PR #337](https://github.com/gap-packages/Semigroups/pull/337).
* There was a bug in the kernel module that related to computing with
  bipartitions, which would sometime cause an unexpected error. This was
  reported by [Wilf A. Wilson][] and Finn Smith in
  [Issue #444](https://github.com/gap-packages/Semigroups/issues/444), and fixed
  by Finn Smith in
  [PR #466](https://github.com/gap-packages/Semigroups/pull/466).
* The Semigroups package would sometimes automatically set the
  `GeneratorsOfSemigroup` of a trivial group of partial permutations to be an
  empty list. This was reported and fixed by [Wilf A. Wilson][] in
  [PR #565](https://github.com/gap-packages/Semigroups/pull/565).
* The function `IsMonogenicSemigroup` incorrectly returned `true` for an empty
  semigroup. This was reported and fixed by [Wilf A. Wilson][]
  in [PR #567](https://github.com/gap-packages/Semigroups/pull/567).

There have also been many technical changes to the package and its kernel
module, including many by [Max Horn][], such as
those in PRs
[#490](https://github.com/gap-packages/Semigroups/pull/490),
[#491](https://github.com/gap-packages/Semigroups/pull/491),
[#493](https://github.com/gap-packages/Semigroups/pull/493),
[#501](https://github.com/gap-packages/Semigroups/pull/501),
[#553](https://github.com/gap-packages/Semigroups/pull/553), and
[#562](https://github.com/gap-packages/Semigroups/pull/562).

Semigroups now requires version 4.8.1 of the
[orb package](https://gap-packages.github.io/orb), or newer.


### Version 3.0.20 (released 01/10/2018)

This version contains some minor changes related to compatibility issues with
GAP, was updated to use [libsemigroups][] 0.6.4, and contains bugfixes for the
following issues:

* [Issue 530](https://github.com/gap-packages/Semigroups/issues/530): there was
  a bug in the method for `IsFactorisableInverseMonoid` for an ideal of
  semigroup. It only returned `true` if the parent semigroup was factorisable.
    [[James D. Mitchell][]]

* [Issue 531](https://github.com/gap-packages/Semigroups/issues/531): there
  were some issues with method selection for Green's classes, when the
  [RCWA package][] was loaded at the same time as [Semigroups][].
    [[Wilf A. Wilson][]]

* [Issue 532](https://github.com/gap-packages/Semigroups/issues/532): the
  `Iterator` method for free inverse monoids used some implementational details
  of the method for free group iterators in the GAP library. This caused an
  error to be reported when [Semigroups][] and the [RCWA package][] were loaded
  at the same time.
    [[James D. Mitchell][]]

* [Issue 536](https://github.com/gap-packages/Semigroups/issues/536): the
  `Iterator` method for free inverse monoids did not work as intended, because
  some words could never be reached.
    [[James D. Mitchell][]]

* [Issue 543](https://github.com/gap-packages/Semigroups/issues/543): the
  [Semigroups][] tests failed when all packages were loaded.
    [[Wilf A.  Wilson][]]

### Version 3.0.19 (released 18/09/2018)

This version contains some minor changes related to compatibility issues with
GAP, and to resolve some incorrectly delegating methods for print/view/display.
([Wilf A. Wilson][])

### Version 3.0.18 (released 11/09/2018)

This version contains some minor changes related to compatibility issues with
GAP.

### Version 3.0.17 (released 23/08/2018)

This version contains some minor improvements and bugfixes.

### Version 3.0.16 (released 29/05/2018)

This version contains some minor improvements and bugfixes.

### Version 3.0.15 (released 24/03/2018)

This version contains some bugfixes, and some improved functionality:
* [Issue 444](https://github.com/gap-packages/Semigroups/issues/444): there was
  a garbage collection error in the kernel module that sometimes resulted in a
  GAP error `"\<func\> must return a value"`.
    [Finn Smith]
* [Issue 459](https://github.com/gap-packages/Semigroups/issues/459): the method
  for `IsTrivial` did not check if the number of generators was non-zero.
    [[James D. Mitchell][]]
* [PR 457](https://github.com/gap-packages/Semigroups/pull/457): an improved
  method for finding a set of generating pairs of a congruence over a Rees
  0-matrix semigroup represented as a linked triple
    [[Michael Young][]]
* [Issue 461](https://github.com/gap-packages/Semigroups/issues/461):
  `NrCongruenceClasses` sometimes returned the wrong result for classes of
  semigroups not defined in the [Semigroups][] package.
    [[Michael Young][]]
* [Issue 463](https://github.com/gap-packages/Semigroups/pull/463): replaced use
  of `EvalString` by `ValueGlobal`.
    [[James D. Mitchell][]]

### Version 3.0.14 (released 15/02/2018)

This version contains a fix for the following issue, this was resolved by
updating the version of [libsemigroups][] to 0.6.3:

* [Issue 450](https://github.com/gap-packages/Semigroups/issues/450): There was
  an intermittent error in some methods for congruences and finitely presented
  semigroups arising from a bug in libsemigroups.
    [[James D. Mitchell][]]

### Version 3.0.13 (released 08/02/2018)

This version contains some minor updates for compatibility with [GAP][] 4.9, and
some fixes for some minor issues.

### Version 3.0.12 (released 17/01/2018)

This version contains some minor updates for compatibility with [GAP][] 4.9, to
fix some bugs, to update the build system, and to update the kernel module for
[libsemigroups][] version 0.6.2.

The following improvements have been made:

* [PR 426](https://github.com/gap-packages/Semigroups/pull/426): a new faster
  method for `IsomorphismFpSemigroup` for factorisable inverse monoids is added.
    [Chris Russell]
* [PR 430](https://github.com/gap-packages/Semigroups/pull/430): the performance
  of the maximal subsemigroups functionality has been improved.
    [[Wilf A.  Wilson][]]

The following issues are resolved:

* [Issue 424](https://github.com/gap-packages/Semigroups/issues/424): the
  operation `MinimalFactorization` sometimes returned incorrect results.
    [[James D. Mitchell][]]
* [Issue 435](https://github.com/gap-packages/Semigroups/issues/435): the
  operation `ClosureInverseMonoid` sometimes returned incorrect results.
    [[James D. Mitchell][]]

### Version 3.0.11 (released 18/12/2017)

This version contains some minor updates for compatibility with [GAP][] 4.9,
the build system has been modified slightly, the kernel module has been
updated for [libsemigroups][] version 0.6.1.

### Version 3.0.10 (released 04/12/2017)

This version contains some minor updates for compatibility with [GAP][] 4.9.

### Version 3.0.9 (released 24/11/2017)

This version contains some minor bugfixes, and updates for compatibility with
[GAP][] 4.9, [io][] 4.5.0, and [Digraphs][] 0.11.0.

### Version 3.0.8 (released 10/11/2017)

This version contains some minor bugfixes, and updates for compatibility with
[GAP][] 4.9 and [orb][] 4.8.0.

### Version 3.0.7 (released 02/10/2017)

This version contains some minor bugfixes, fixes some issues where some tests in
the main GAP repo returned different output when Semigroups was loaded than when
it was not, and updates the kernel module for version 0.5.2 of
[libsemigroups][]. The configuration option `--enable-debug` was added.

The following issues are resolved:

* [Issue 389](https://github.com/gap-packages/Semigroups/issues/389): the most
  general method for `NaturalPartialOrder` sometimes returned incorrect results.
    [[Wilf A. Wilson][]]
* [Issue 393](https://github.com/gap-packages/Semigroups/issues/393):
  `StructureDescription` for finitely presented groups failed with an error when
  Semigroups was loaded.
    [[James D. Mitchell][]]
* [Issue 395](https://github.com/gap-packages/Semigroups/issues/395): GAP's test
  `tst/testinstall/semigrp.tst` failed because of a missing method for
  `NrEquivalenceClasses` for a generic semigroup congruence.
    [[James D. Mitchell][] and [Wilf A. Wilson][]]

### Version 3.0.6 (released 27/09/2017)

This version contains some minor bugfixes, improves the compatibility of
Semigroups with other GAP packages, and updates the kernel
module for version 0.5.0 of [libsemigroups][]

The following issues are resolved:
* [Issue 371](https://github.com/gap-packages/Semigroups/issues/371): the
  identity element of some types of monoids was not added to its
  `GeneratorsOfSemigroup`. This meant that the semigroup generated by
  `GeneratorsOfSemigroup(M)` was not equal to `M` in some rare cases.
    [[Wilf A.  Wilson][]]
* [Issue 377](https://github.com/gap-packages/Semigroups/issues/377): there was
  a bug in the method for `IsInverseSemigroup` for non-acting semigroups that
  sometimes returned a false positive.
    [[Wilf A. Wilson][]]

### Version 3.0.5 (released 23/08/2017)

This version contains some minor tweaks and the following issue is resolved:
* [Issue 352](https://github.com/gap-packages/Semigroups/issues/352): There was
  a name clash with some other GAP packages using `RandomMatrix` and
  `IsTorsion`.
    [[James D. Mitchell][]]

### Version 3.0.4 (released 16/07/2017)

Some minor issues are fixed in this release:
* [Issue 342](https://github.com/gap-packages/Semigroups/issues/342):
  `DirectProduct` for transformation semigroups returned the wrong answer when
  applied to semigroups satisfying `IsMonoidAsSemigroup`.
    [[James D. Mitchell][]]

Some documentation and tests were added by [Michael Young][].

### Version 3.0.3 (released 21/06/2017)

Some minor issues are fixed in this release:
* [Issue 336](https://github.com/gap-packages/Semigroups/issues/336): Rees
  (0-)matrix semigroups over non-permutation groups sometimes resulted in an
  error.
    [[James D. Mitchell][]]
* A method was added for `IsEUnitaryInverseSemigroup` for non-inverse
  semigroups, which previously resulted in no method found.
    [Chris Russell]
* Some error messages were improved in `ReadGenerators` and `WriteGenerators`.
    [[James D. Mitchell][]]

### Version 3.0.2 (released 16/06/2017)

This is an minor release fixing some minor issues in the last release.

The following issues were resolved:
* [Issue 330](https://github.com/gap-packages/Semigroups/issues/330):
  `InversesOfSemigroupElement` some times returned an incorrect value,
  specifically when applied to the identity of a transformation monoid.
    [[James D. Mitchell][]]
* [Issue 328](https://github.com/gap-packages/Semigroups/issues/328): when using
  Linux the package compiled but failed to link pthreads and so the kernel
  module failed to load in GAP.
    [[James D. Mitchell][]]

There are improvements to the following:
* some missing documentation was added by [Michael Young][].
* the subsemigroup returned by `IdempotentGeneratedSubsemigroup` for Rees
  (0-)matrix semigroup over a group has a smaller generating set than
  previously, and can be found more quickly.
    [[Wilf A. Wilson][]]
* `IsomorphismSemigroups` is extended so that it can be applied to arbitrary
  simple, 0-simple, or monogenic semigroups.
    [[Wilf A. Wilson][]]

### Version 3.0.1 (released 03/06/2017)
This is an extremely minor release fixing some minor issues in the last
release.

### Version 3.0.0 (released 02/06/2017)
This is a major release that dramatically expands the scope of the package.  The
package now features a compiled C/C++ module which interfaces with the
[libsemigroups][] C++ library to allow high-speed computations for congruences
and certain categories of semigroup.  There are also several new types of
semigroup and a variety of new methods which can be used with them.

### Version 2.8.2 (released 15/01/2018)
This is a minor release to correct the required version of [GAP][] in
`PackageInfo.g` (from GAP 4.9.0 to GAP 4.8.9).

### Version 2.8.1 (released 22/12/2017)
This is a minor release to update the output in some test file and manual
examples due to some changes in the GAP library code.

### Version 2.8.0 (released 26/05/2016)
In this release there are some new features and some bug fixes. In this
version, we welcome [Nick Ham][] to the contributors to the package.

#### New Features in Version 2.8.0
The new features in this release are contributed by [Nick Ham][]:

* `ApsisMonoid`
* `CrossedApsisMonoid`
* `ModularPartitionMonoid`
* `PlanarModularPartitionMonoid`
* `PlanarPartitionMonoid`
* `PlanarUniformBlockBijectionMonoid`
* `SingularApsisMonoid`
* `SingularCrossedApsisMonoid`
* `SingularModularPartitionMonoid`
* `SingularPlanarModularPartitionMonoid`
* `SingularPlanarPartitionMonoid`
* `SingularPlanarUniformBlockBijectionMonoid`
* `SingularUniformBlockBijectionMonoid`
* `UniformBlockBijectionMonoid`

#### Issues Resolved in Version 2.8.0

* [Issue 160](https://github.com/gap-packages/Semigroups/issues/160)
  `IrreundantGeneratingSubset` behaved incorrectly when given a semigroup whose
  generating set consisted of a single repeated element.
    [[Wilf A. Wilson][]]
* [Issue 164](https://github.com/gap-packages/Semigroups/issues/164)
  `MatrixEntries` gave an error for Rees 0-matrix semigroups whose matrices
  contain `0`.
    [[Wilf A. Wilson][]]
* Some tests failed when GAP was compiled in 32-bit mode.
    [[Michael Young][]]

### Version 2.7.6 (released 19/04/2016)
This is a very minor release changing the name of the `README` (to `README.md`)
in the `PackageInfo.g` file.

### Version 2.7.5 (released 19/04/2016)
This is a minor release to fix
[Issue 151](https://github.com/gap-packages/Semigroups/issues/151), and to make
some changes for future compatibility with GAP. In
[Issue 151](https://github.com/gap-packages/Semigroups/issues/151) when the
method `IsomorphismPermGroup` was applied to a semigroups of non-permutation
transformations the returned mapping was not an isomorphism.

### Version 2.7.4 (released 02/03/2016)
This is a minor release to fix
[Issue 150](https://bitbucket.org/james-d-mitchell/semigroups/issue/150), and to
correct the required version of GAP (from 4.8.2 to 4.8.3).
In [Issue 150](https://bitbucket.org/james-d-mitchell/semigroups/issue/150) the
function `IsZeroSimpleSemigroup` entered an infinite loop for some examples of
semigroups of partial permutations.

### Version 2.7.3 (released 15/02/2016)
This is a minor release to fix some manual examples, to correct the package URL
in the `PackageInfo.g` file, and to fix some issues with semigroups of
bipartitions. It was formerly possible to create semigroups of bipartitions
where the generators had different degrees, but the created semigroups were
invalid; this is fixed in version 2.7.3.

### Version 2.7.2 (released 28/01/2016)

This is a minor release to fix to remove `ErrorMayQuit` which was renamed
`ErrorNoReturn` in GAP 4.8.2. This change was made by [Max Horn][].

### Version 2.7.1 (released 19/12/2015)

This is a minor release to fix
[Issue 144](https://bitbucket.org/james-d-mitchell/semigroups/issue/144).  This
issue resulted in `IsInverseSemigroup` sometimes returning `true` for semigroups
which were not inverse. This occurred when the \\(\mathscr{D}\\)-classes of the
semigroup were computed before the method for `IsInverseSemigroup` was first
run.

### Version 2.7 (released 27/11/2015)

This is a minor release including some changes for compatibility with GAP 4.8,
and some bug fixes.

#### New Features in Version 2.7
* `IsomorphismReesZeroMatrixSemigroup` is introduced, and it is no longer
  possible to apply `IsomorphismReesMatrixSemigroup` to a 0-simple semigroup.
  This change was made for the sake of consistency, so that the `Range` of an
  `IsomorphismReesMatrixSemigroup` is always a Rees matrix semigroup and not
  sometimes a Rees 0-matrix semigroup as was formerly the case.

#### Changes for GAP 4.8
* several `ViewString` methods for semigroups and their elements were moved from
  the `Semigroups` package to the GAP library. Some minor changes were made in
  the method for `ViewString` for semigroups, and the tests, and manual examples
  were updated accordingly.
* The meaning of `IsMonoidAsSemigroup` was changed to be consistent with the
  meaning of `IsGroupAsSemigroup`. In earlier versions, `IsMonoidAsSemigroup`
  was `false` for semigroups in the category `IsMonoid`. From Version 2.7,
  `IsMonoidAsSemigroup` is `true` for monoids in the category `IsMonoid` and for
  some further semigroups.

#### Issues Resolved in Version 2.7

* [Issue 136](https://bitbucket.org/james-d-mitchell/semigroups/issue/136):
  `CyclesOfPartialPermSemigroup` sometimes resulted in an error due to using
  `DegreeOfPartialPermSemigroup` instead of the maximum of the degree and the
  codegree.
    [James D. Mitchell][]
* [Issue 141](https://bitbucket.org/james-d-mitchell/semigroups/issue/141):
  `PartialOrderOfDClasses` sometimes resulted in an error. This bug was
  introduced in Semigroups 2.6 and did not effect any previous versions.
    [James D. Mitchell][]

### Version 2.6 (released 22/09/2015)

This release includes some bugfixes, some minor new features, and one major new
feature (efficient methods for semigroups of matrices over a finite field).

#### New Features in Version 2.6

* extensive new features for computing with semigroups, monoids, and ideals, of
  matrices with entries in a finite field.  See Chapter 7 of the manual for more
  details.
    [[Markus Pfeiffer][]]

* The functions `RectangularBand`, `MonogenicSemigroup`, and `ZeroSemigroup` now
  have an optional first argument to specify the category of the result; the
  functions `LeftZeroSemigroup` and `RightZeroSemigroup` are introduced in a
  similar sense.
    [[Wilf A. Wilson][]]

* The new property `IsSemigroupWithAdjoinedZero` and attribute
  `UnderlyingSemigroupOfSemigroupWithAdjoinedZero` are introduced.
    [[Wilf A. Wilson][]]

* The operations `MotzkinMonoid` and `PartialJonesMonoid` were introduced.
    [James D. Mitchell][]

#### Issues Resolved in Version 2.6

* [Issue 131](https://bitbucket.org/james-d-mitchell/semigroups/issue/131):
  testing membership in a Rees 0-matrix semigroup that knows it is inverse
  sometimes resulted in an error.
    [[Michael Young][]]

* [Issue 132](https://bitbucket.org/james-d-mitchell/semigroups/issue/132): this
  was a feature request to introduce the operations `MotzkinMonoid` and
  `PartialJonesMonoid`.
    [James D. Mitchell][]

* [Issue 134](https://bitbucket.org/james-d-mitchell/semigroups/issue/134): the
  operation `PartialBrauerMonoid` returned the wrong answer when the argument
  was `1`. The returned semigroup was not the partial brauer monoid of degree 1.
    [James D. Mitchell][]

### Version 2.5 (released 01/06/2015)

This is a minor release including several bugfixes, lots of minor improvements
in the documentation, some improvements in performance, and some new features.

#### New Features in Version 2.5

* Semigroups of partial permutations now have a polynomial time (quadratic in
  the degree) algorithm for computing the minimal ideal
    [[Wilf A. Wilson][]]

* A more efficient `IsInverseSemigroup` method for Rees 0-matrix semigroups is
  introduced, along with new methods for `Idempotents` and `NrIdempotents` for
  inverse Rees 0-matrix semigroups
    [[Wilf A. Wilson][]]

* The documentation for congruences has been improved and new tests have been
  added.
    [[Michael Young][]]

* A `UniversalSemigroupCongruence` now returns a much smaller set of generating
  pairs.
    [[Michael Young][]]

#### Issues Resolved in Version 2.5

Issue numbers refer to those on the [Bitbucket issue tracker][].

* [Issue 126](https://bitbucket.org/james-d-mitchell/semigroups/issue/126):
  testing membership in a Rees 0-matrix semigroup that knows it is inverse
  sometimes resulted in an error.
    [James D. Mitchell][]

* [Issue 127](https://bitbucket.org/james-d-mitchell/semigroups/issue/127): the
  main algorithm for computing with ideals of acting semigroups which know they
  are regular contained a bug that resulted in incorrect results. In some
  cases, some \\(\mathscr{D}\\)-classes were counted more than once, and the
  returned value of `Size` was higher than the actual size of the ideal.
    [James D. Mitchell][]

* [Issue 128](https://bitbucket.org/james-d-mitchell/semigroups/issue/128): in
  some special cases `UnderlyingSemigroup`, `ViewObj`, `Size`, and related
  methods, for Rees 0-matrix semigroups over non-groups returned an error.
    [James D. Mitchell][]

* The universal congruence specified by generating pairs on a (0-)simple
  semigroup no longer causes an error.
    [[Michael Young][]]

### Version 2.4.1 (released 15/05/2015)
This is a extremely minor release to change 1 character in the PackageInfo.g
file (wrong package archive URL).

### Version 2.4 (released 02/04/2015)
This is a minor release including several bugfixes, and improvements in
performance, and some new features.

#### New Features in Version 2.4

* The function `RepresentativeOfMinimalIdeal` is introduced.
    [[Wilf A. Wilson][]]

* Transformation semigroups now have a polynomial time (cubic in the degree)
  algorithm for computing the minimal ideal
    [[Wilf A. Wilson][]]

* The functions `RectangularBand`, `ZeroSemigroup`, and `MonogenicSemigroup` are
  introduced.
    [[Wilf A. Wilson][]]

* A method for choosing a random element of a semigroup has been introduced in
  the case that the semigroup knows its set of elements. This new method choose
  elements at random with uniform probability.
    [[Wilf A. Wilson][]]

* The documentation and tests for congruences has been improved.
    [Wilf A. Wilson][]] and [[Michael Young][]

* The functionality for Rees congruences has been rewritten and improved.
    [[Michael Young][]]

* There is a new `Enumerator` method for congruence classes of a semigroup
  congruence.
    [[Michael Young][]]

#### Issues Resolved in Version 2.4

Issue numbers refer to those on the [Bitbucket issue tracker][].

* [Issue 88](https://bitbucket.org/james-d-mitchell/semigroups/issue/88):
  an inefficiency in `JoinIrreducibleDClasses` of an inverse semigroup ideal
  resulted in a call to `GeneratorsOfSemigroup`.

* [Issue 94](https://bitbucket.org/james-d-mitchell/semigroups/issue/94):
  `EquivalenceClasses` of the trivial congruence (generated by 0 pairs of
  elements) returned an error.

* [Issue 95](https://bitbucket.org/james-d-mitchell/semigroups/issue/95): The
  class containing the zero element of a Rees 0-matrix semigroup was not
  returned by `EquivalenceClasses` of a congruence over a Rees 0-matrix
  semigroup.

* [Issue 108](https://bitbucket.org/james-d-mitchell/semigroups/issue/108):
  `IsRegularSemigroup` with argument a Rees 0-matrix semigroup returned an
  error.

* [Issue 119](https://bitbucket.org/james-d-mitchell/semigroups/issue/119):
  `NrCongruencesClasses` and related methods did not work for Rees congruences.

* [Issue 121](https://bitbucket.org/james-d-mitchell/semigroups/issue/121):
  `MultiplicativeZero` and `IsMultiplicativeZero` sometimes returned incorrect
  results when applied to a non-acting semigroup (i.e. a semigroup not of
  transformations, partial permutations, partitions, or subsemigroups of a Rees
  0-matrix semigroup).

* [Issue 122](https://bitbucket.org/james-d-mitchell/semigroups/issue/122): A
  bug in the creation of Green's classes of ideals of semigroups, which resulted
  in an error.

* [Issue 123](https://bitbucket.org/james-d-mitchell/semigroups/issue/123):
  `IsZeroSemigroup` sometimes returned a false positive when applied to a
  non-acting semigroup.

### Version 2.3 (released 16/03/2015)

This is a minor release including some internal refactoring, and
subsequent bugfixes, and stability improvements.

* [Issue 116](https://bitbucket.org/james-d-mitchell/semigroups/issue/116) was
  resolved. In some cases when the default length of hash tables in
  [Semigroups][] was set to be very small, a segmentation fault occurred. This
  is a bug in the [orb package][] (see
  [Issue 10](https://github.com/gap-packages/orb/issues/10)), but we worked
  around it to resolve this issue.

### Version 2.2 (released 20/01/2015)
This is a minor release including some bug fixes, performance
improvements, and additional functionality.

#### New Features in Version 2.2

* The functions `SmallestElementSemigroup`, `LargestElementSemigroup`,
  and `GeneratorsSmallest`.

* Free bands are introduced.

* Error messages are more uniform.

* The function `RegularDClasses` was introduced to resolve
  [Issue 102](https://bitbucket.org/james-d-mitchell/semigroups/issue/102).

* The documentation and code for semigroup congruences has been improved, and is
  better integrated with the core [GAP][] system.

* The functions `ReadGenerators` and `WriteGenerators` were improved.

#### Issues Resolved in Version 2.2

* Some minor corrections were made to the methods for creating the ideals of
  some semigroups in standard examples semigroups, such as
  `SingularTransformationSemigroup`.

* [Issue 102](https://bitbucket.org/james-d-mitchell/semigroups/issue/102): we
  introduced `RegularDClasses`.

* [Issue 104](https://bitbucket.org/james-d-mitchell/semigroups/issue/104): the
  performance of `MaximalSubsemigroups` when applied to an inverse semigroup has
  been improved.

* [Issue 105](https://bitbucket.org/james-d-mitchell/semigroups/issue/105):
  `CyclesOfPartialPerm` is now documented and does not return nonsense.

* [Issue 106](https://bitbucket.org/james-d-mitchell/semigroups/issue/106):
  `MaximalSubsemigroups` sometimes failed when the ResClasses package was
  loaded. We refactored the code so that the method from ResClasses is no longer
  applied.

* [Issue 107](https://bitbucket.org/james-d-mitchell/semigroups/issue/107): A
  bug in the creation of Green's classes of an ideal of a semigroup, which
  sometimes caused an error, has been resolved.  This issue often caused
  `MaximalSubsemigroups` to stop in an error.

* [Issue 110](https://bitbucket.org/james-d-mitchell/semigroups/issue/110):
  `MaximalSubsemigroups` can be applied to any class of semigroup where it is
  possible to find an isomorphism to a transformation semigroup.

* [Issue 111](https://bitbucket.org/james-d-mitchell/semigroups/issue/111):
  `POPI(1)` returned the wrong semigroup. Similar issues existed in other corner
  cases, and these have been resolved too.

### Version 2.1.1 (released 09/09/2014)

This is a very minor release to fix an issue caused by only loading the packages
needed (but not required) by [Semigroups][].

### Version 2.1 (released 04/09/2014)

This is a minor release including some bug fixes and performance improvements.

#### New Features in Version 2.1

* The functions:

  - `AsTransformationSemigroup`,
  - `AsPartialPermSemigroup`,
  - `AsBipartitionSemigroup`,
  - `AsBlockBijectionSemigroup`

  which are shortcuts to `Range(IsomorphismXSemigroup(S))`.

* A method for `IsTransitive` for a transformation semigroup and, optionally, a
  positive integer or list of positive integers. This method is based on Gabow's
  algorithm for determining the strongly connected components of a directed
  graph.

* The functions `MeetSemigroupCongruence` and `JoinSemigroupCongruences` for
  finding the meet and join of a pair of congruences of a semigroup.

* There is a new method for `IsSynchronizingSemigroup`, suggested by Peter
  Cameron, with better complexity than the previous method.

#### Issues Resolved in Version 2.1

Issue numbers refer to those on the [Bitbucket issue tracker][].

* There was a bug in `ReadGeneratorsFile`, which meant it sometimes returned
  fail. The mode argument for `IO_FilteredFile` was not given.

* There was a bug in the `\in` method for a congruence of a Rees 0-matrix
  semigroup, which sometimes returned the wrong answer for the zero of the
  semigroup.

* There was a bug in `IteratorFromGeneratorsFile` that caused it to read only
  every other line in the given file, and to crash if there were an odd number
  of lines.

* There was no hash function for bipartitions.

* The documentation for `InverseSubsemigroupByProperty` did not specify the
  arguments of the function.

* [Issue 82](https://bitbucket.org/james-d-mitchell/semigroups/issue/82): it is
  now possible to take the quotient of a semigroup by an ideal using the `/`
  operator.

* [Issue 96](https://bitbucket.org/james-d-mitchell/semigroups/issue/96):
  `IsIsomorphicSemigroup` sometimes returned a false negative by incorrectly
  comparing the output of `PartialOrderOfDClasses` (up to isomorphism) rather
  than the transitive reflexive closure of `PartialOrderOfDClasses`.

* [Issue 97](https://bitbucket.org/james-d-mitchell/semigroups/issue/97): there
  was a bug in the `Normalizer` method, which caused [GAP][] to crash when the
  argument was a monoid with 0 generators.

* [Issue 98](https://bitbucket.org/james-d-mitchell/semigroups/issue/98):
  `PartitionMonoid(1)` returned the wrong answer, it was missing the
  non-identity element.

* [Issue 99](https://bitbucket.org/james-d-mitchell/semigroups/issue/99): the
  documentation for `PartialOrderOfDClasses` was incorrect.

* [Issue 103](https://bitbucket.org/james-d-mitchell/semigroups/issue/103):
  under certain circumstances an error was given when trying to compute with an
  ideal of an inverse semigroup.

### Version 2.0 (released 17/04/2014)

This is a major release including many new features and several bug fixes.

#### New Features in Version 2.0

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

* the new operations `IsomorphismSemigroups`, `IsIsomorphicSemigroup`,
and `SmallestMultiplicationTable`. Some of the methods for this
operation require the Grape package to be fully installed;

* the new operation `MaximalSubsemigroups`, which returns the maximal
subsemigroups of an arbitrary semigroup. Some of the methods for
this operation require the Grape package to be fully installed;

* the operation `IsMaximalSubsemigroup`;

* the new operation `Normalizer` for computing a subgroup of a
permutation group consisting of those permutations that stabilise,
under conjugation, a transformation, partial perm, or bipartition
semigroup. The genss package is required for this operation in some
cases;

* the new operation `CharacterTableOfInverseSemigroup` for finding the
character table of an inverse semigroup of partial permutations;

* methods for defining and manipulating the congruences of a Rees
0-matrix semigroup;

* the properties `IsCongruenceFreeSemigroup`, `IsEUnitaryInverseSemigroup`;

* the attributes:
  * `ComponentRepsOfTransformationSemigroup`
  * `ComponentsOfTransformationSemigroup`
  * `CyclesOfTransformationSemigroup`
  * `ComponentRepsOfPartialPermSemigroup`
  * `ComponentsOfPartialPermSemigroup`
  * `CyclesOfPartialPermSemigroup`.

* the new function `IteratorFromGeneratorsFile` that returns an
iterator which reads semigroup elements from a file created using
`WriteGenerators`. This function is a convenient way of, for example,
looping over a collection of generators in a file without loading
every object in the file into memory. This  might be useful if
the file contains more information than there is available memory;

* the operation `EndomorphismsPartition` that returns the monoid
of endomorphisms preserving a partition. This monoid is defined
using the minimum possible number of generators;

* a version of the function `Splash` that attempts to convert a string
containing a dot or tikz document into a pdf and opens this pdf.
Other file formats are also supported;

* the function `DotSemilatticeOfIdempotents` that produces a string
containing a dot document of the semilattice of idempotents of an
inverse semigroup grouped by \\(\mathscr{D}\\)-class;

* the operation `NaturalLeqInverseSemigroup`, which is an umbrella
operation for `NaturalLeqPartialPerm`, and other such functions.

#### Issues Resolved in Version 2.0

Issue numbers refer to those on the [Bitbucket issue tracker][].

* the main algorithm underlying many of the methods in [Semigroups][] has been
  revised to avoid computing the same information more than once. Some further
  internal rearranging and cleaning up was done.

* `MinimalIdeal` and `SingularTransformationSemigroup` now returns an ideal
  rather than a semigroup defined by a generating set;

* to reduce the size of the package archive, the examples directory has been
  removed. The content of the examples directory is available on
  [this webpage](https://tinyurl.com/jdmitchell/data.php).

* several bugs in the setup for subsemigroups of Rees 0-matrix semigroups were
  resolved. These issues would have caused [GAP][] to give an error in certain
  circumstances.

* [Issue 33](https://bitbucket.org/james-d-mitchell/semigroups/issue/33): an
  error was returned when trying to calculate the size, or multiply elements in
  the quotient of a semigroup by an ideal.

* [Issue 36](https://bitbucket.org/james-d-mitchell/semigroups/issue/36),
  [Issue 64](https://bitbucket.org/james-d-mitchell/semigroups/issue/64): the
  function `SmallGeneratingSet` was ambiguous, in the sense that it was
  sometimes unclear how to recreate a semigroup from its small generating set.
  For example, `SmallGeneratingSet` of a monoid could return an empty list, but
  this empty list could not be used to recreate the monoid in [GAP][]. This was
  resolved by introducing the functions `SmallSemigroupGeneratingSet`,
  `SmallMonoidGeneratingSet`, `SmallInverseSemigroupGeneratingSet`,
  `SmallInverseMonoidGeneratingSet`.  These functions can also now be applied to
  collections of elements, i.e. not only to semigroups.

* [Issue 47](https://bitbucket.org/james-d-mitchell/semigroups/issue/47):
  `ClosureSemigroup` had several bugs that could, in some cases, result in
  incorrect results, or semigroups with invalid data structures.

* [Issue 50](https://bitbucket.org/james-d-mitchell/semigroups/issue/50) and
  [Issue 59](https://bitbucket.org/james-d-mitchell/semigroups/issue/59):
  `WriteGenerators` wrote nothing to a file in the case that it was not piping
  through `xz` or `gzip`.

* [Issue 55](https://bitbucket.org/james-d-mitchell/semigroups/issue/55):
  `DotDClasses` did not work when the argument was a Rees 0-matrix semigroup (it
  worked as intended when the argument was a subsemigroup of such a semigroup
  defined by a generating set).

* [Issue 56](https://bitbucket.org/james-d-mitchell/semigroups/issue/56): the
  functions `Monoid` and `InverseMonoid` sometimes did not contain their
  identity element.

* [Issue 57](https://bitbucket.org/james-d-mitchell/semigroups/issue/57): under
  certain circumstances a bug in the [GAP][] kernel function `INV_KER_TRANS`,
  that didn't handle kernels and transformations with different length and
  degree properly, caused [GAP][] to give an error.

* [Issue 63](https://bitbucket.org/james-d-mitchell/semigroups/issue/63): there
  was an error in the [GAP][] library functions `Monoid` and `InverseMonoid`,
  when they were passed a monoid as an argument.

* [Issue 63](https://bitbucket.org/james-d-mitchell/semigroups/issue/63) (and
  [Issue 4](https://github.com/gap-system/orb/issues) in the [orb package][]): a
  bug in the [orb package][] meant that the log of an `Orb` was not properly
  updated if the enumeration stopped early because we found something we were
  looking for. This caused [Semigroups][] to return incorrect results in some
  rare cases.

* [Issue 72](https://bitbucket.org/james-d-mitchell/semigroups/issue/72): the
  method for `IsomorphismTransformationSemigroup` applied to a binary relation
  monoid returned an isomorphism to a transformation semigroup which was missing
  the image of the identity.

* [Issue 89](https://bitbucket.org/james-d-mitchell/semigroups/issue/89): there
  was a bug in `TRANS_IMG_CONJ` which failed to handle transformations of
  unequal degrees correctly. This causes incorrect results to be returned when
  considering semigroups generated by transformations of unequal degrees.

### Version 1.4 (released 28/10/13)

This is a minor release containing some bug fixes. Specifically:
the functionality of `ReadGenerators` and `WriteGenerators` has been
improved to allow the argument to be an [io][] file object, and support
is added to read and write directly to files compressed using
`xz`. A minor bug relating to the creation of idempotents in
transformation semigroups which was triggered by the identity
transformation has been resolved. The functions
`IsomorphismReesMatrixSemigroup`, `InjectionPrincipalFactor`, and
`IsZeroSimpleSemigroup` have been revised. `IsZeroSimpleSemigroup`
formerly returned `true` for the 2-element zero semigroup, which is
not 0-simple. `IsomorphismReesMatrixSemigroup` could have returned an
error if called for a semigroup which was not a semigroup of
partial perms or transformations. The use of `AsPermutation` was
changed to `PermutationOfImage` where appropriate following a change
to the library methods for `AsPermutation`. The declarations of
`IsomorphismPermGroup` and `ClosureSemigroup` were moved/changed to
avoid warnings that their methods matched more than one
declaration. These warnings were exposed by doing `LoadAllPackages`,
but were not present when loading [Semigroups][] by itself.

### Version 1.3 (released 11/10/13)

Version 1.3 contains many bug fixes, extensions and improvements in
the documentation, and several new methods and functions. Most
notably are (in no particular order):

* the methods in [Semigroups][] have been extended to apply to arbitrary
  subsemigroups of regular Rees 0-matrix semigroups over groups;
* a new method for `MaximalSubsemigroups` of Rees matrix semigroup has been
  implemented;
* the functions `Read/WriteSemigroups` have been renamed `Read/WriteGenerators`,
  and their performance has been improved. It is now possible to use
  `WriteGenerators` to write to a gzipped file;
* the operation `SingularSemigroup` has been renamed to
  `SingularTransformationSemigroup`;
* the following attributes have been introduced:
  * `MinimalDClass`,
  * `MaximalDClasses`,
  * `StructureDescriptionMaximalSubgroups`,
  * `StructureDescriptionSchutzenbergerGroups`, and
  * `IsGreensDLeq`.
* the attribute/operation `DotDClasses` has been introduced. This allows the
  \\(\mathscr{D}\\)-class diagram of a semigroup to be viewed.
* `ComponentRepsOfTransformationSemigroup` is reintroduced.

### Version 1.2 (released 02/08/13)

This release includes several new methods for inverse semigroups of
partial permutations and for free inverse semigroups. Most notably
among the new methods for inverse semigroups of partial
permutations are:

* `SmallerDegreePartialPermRepresentation`
* `VagnerPrestonRepresentation`

for changing the representation of an inverse semigroup of partial
permutations. The changes in this release were the result of the
University of St Andrews Research for Undergraduates Summer School
in 2012, and were largely written by [Wilf A. Wilson][] and Robert Hancock.

Free inverse semigroups, and their elements, are also introduced, this part of
the package was written by [Julius Jonusas][] (who wishes to acknowledge the
support of the Carnegie Trust).

### Version 1.1 (released 11/06/13)
A minor release to fix some technical issues in `PackageInfo.g`, the
declarations of `IsGreens.Class`, and a minor change in the output in
one test in everyfunction.tst which was consequence of the
declarations of `IsGreens.Class`.

### Version 1.0 (released 07/06/13)
The package has been renamed from Citrus to [Semigroups][].
The package has been completely overhauled, the performance has been improved,
and the code has been generalized so that in the future the same code can be
used to compute with other types of semigroups.

# Under the name Citrus:

### Version 0.9999
This is the final release of Citrus (the package will be renamed
Citrus in the next release since the scope of the package has
expanded to include more types of semigroups than just those of
transformations and partial permutations).

A minor release fixing several bugs relating to inverse semigroups
of partial permutations pointed out by participants at the
University of St Andrews Research for Undergraduates Summer School
in July 2012.  Most notably by Demi Allen, Casey Donoven, Rhiannon
Dougall, Robert Hancock, and [Wilf A. Wilson][]. More specifically,
`SymmetricInverseSemigroup(n)` returned an incorrect answer when `n=1`
or `n=2`, `\in` for the empty mapping and an inverse semigroup of
partial perms sometimes incorrectly returned false, some harmless
compiler warnings were displayed when using more recent versions of
gcc, `NaturalLeqPP` sometimes returned the incorrect value, there was
no method for `IsInverseSemigroup` or `IsInverseMonoid` for a semigroup
of partial perms.

### Version 0.999
A minor release fixing several bugs relating to partial
permutations and monoids thereof, pointed out by Jack Schmidt.
More specifically, `MultiplicativeZero` sometimes incorrectly
returned fail for an inverse semigroup of partial permutations,
sometimes `PartialPerm` incorrectly returned fail when given a dense
range as an argument, sometimes the size of an inverse monoid was 1
more than the correct value, and `RestrictedPP` sometimes failed when
it should not have.

### Version 0.99
another minor release. Specific changes were: removed the
declaration of SmallGeneratingSet for IsSemigroup since it appears
not to be used and caused a warning to be shown when the [RCWA package][] was
loaded after Citrus. Added a new abstract to the `PackageInfo.g`
file, and the documentation, and updated the webpages, in
particular so that the html version of the manual is linked to that
on the [GAP][] webpage and the links to other manuals work.

### Version 0.9
renamed the function for creating the semigroup of order-preserving
transformations on an n-chain from `O` to `OrderEndomorphisms` after it
was pointed out that it is not sensible to have function names with
one character. Also made some minor adjustments to the manual.

### Version 0.8
minor changes due to incompatibility with [Smallsemi][] version 0.6.4 which
caused some test files to fail when Nilmat was loaded after
Citrus.  The clashes and the failed test were caused by various
properties being declared for `IsTransformationSemigroup` rather than
`IsSemigroup`.

### Version 0.7
the most major change is the introduction of special methods for
partial permutations and inverse semigroups. So that these methods
are efficient, a [GAP][] kernel component (in C) has also been
introduced for various low-level computations with partial
permutations. Essentially all functions previously available for
transformation semigroups are now available for inverse semigroups
of partial permutations. The manual has been expanded and
reorganised, some standard examples have been included (semigroups
of order preserving transformations or partial permutations, the
symmetric inverse semigroup, the full matrix semigroup over a
finite field), the endomorphism monoids of the non-abelian groups
with at most 64 elements have been included in the catalogues of
examples, the functions `InjectionPrincipalFactor`,
`IsomorphismReesMatrixSemigroup`, and `PrincipalFactor`, and some
specific properties and attributes of inverse semigroups have been
introduced (such as `IsFactorisableSemigroup` and
`PrimitiveIdempotents`).

### Version 0.6
fixed a bug relating to the creation of transformation semigroups
using `MagmaByGenerators`. Also added the global variable
`CitrusOptionsRec` containing the default values of the options
used by Citrus when creating a semigroup.

### Version 0.5
major changes are: the documentation has been further revised, functions for
creating semigroups and monoids with certain options have been introduced,
several functions have had the word `Greens` removed from their names to reduce
the length, the operation `ClosureSemigroup` has been introduced, the functions
`ReadCitrus` and `WriteCitrus` for reading and writing transformations to a file
have been introduced, several catalogues of examples of transformation
semigroups are now included in the examples directory, methods for creating a
Green's class inside another Green's class have been included (such as an
\\(\mathscr{R}\\)-class of a \\(\mathscr{D}\\)-class or an
\\(\mathscr{H}\\)-class of an \\(\mathscr{L}\\)-class), the hash functions used
for transformations etc have been improved.

Some minor bugs have been fixed, and new methods or functions with the following
names have also been introduced:

* `AntiIsomorphismTransformationSemigroup` (for a transformation semigroup),
* `IdempotentGeneratedSubsemigp`,
* `IsomorphismTransformationSemigroup` (for a permutation group),
* `IsomorphismTransformationMonoid` (for a permutation group),
* `NrElementsOfRank`.

### Version 0.4
major changes are: the documentation has been updated, some changes
to core functions for \\(\mathscr{R}\\)-classes/image orbits have resulted in a
performance improvement, there is a method for the operation
`Factorization` allowing an arbitrary element of a transformation
semigroup to be expressed as a product of the generators.

Some minor bugs have been fixed, and new methods or functions with
the following names have also been introduced:

`OrbSCC`, `OrbSCCLookup`, `OrbSCCTruthTable`,
`ReverseSchreierTreeOfSCC`, `SchreierTreeOfSCC`,
`IsomorphismTransformationSemigroup` (for a perm. gp).

### Version 0.3
fixed a critical (but rare) bug in AddToOrbitsOfKernels that caused
computations relating to \\(\mathscr{D}\\)-classes, \\(\mathscr{H}\\)-classes,
or \\(\mathscr{L}\\)-classes to return incorrect answers in some cases.

### Version 0.2
updated the method for `\^` for a transformation and perm so that it
is more efficient than the library method, same for \* for a perm
and transformation. New method for `StructureDescription` of a
Brandt semigroup, and `IsSubset` for a trans. semigroup and trans.
coll.

fixed bugs in `IndexPeriodOfTransformation` (it returned incorrect
results) and `AsPermutation`. Also reduce hash table lengths so that
Citrus uses less memory. Fixed bug that triggered an infinite
loop when trying to find elements of a trivial trans. semigroup.

added the functions `CitrusDefaultMem`, `CitrusHiMem`,
`CitrusLoMem`, `CitrusVeryLoMem`, `IsBrandtSemigroup`,
`IsLeftSimple`, `IsMonogenicSemigroup`, `IsRightSimple`,
`IsZeroRectangularBand`, `IsZeroSimpleSemigroup`.

[D. V. Pasechnik]: http://users.ox.ac.uk/~coml0531
[James D. Mitchell]: https://jdbm.me
[Julius Jonusas]: http://julius.jonusas.work
[Markus Pfeiffer]: https://markusp.morphism.de
[Max Horn]: https://www.quendi.de/en/math
[Michael Young]: https://mct25.host.cs.st-andrews.ac.uk
[Nick Ham]: https://n-ham.github.io
[Wilf A. Wilson]: https://wilf.me
[Chris Jefferson]: https://heather.cafe
[Finn Smith]: https://flsmith.github.io
[Luke Elliott]: https://le27.github.io/Luke-Elliott/
[GAP]: https://www.gap-system.org
[Digraphs package]: https://gap-packages.github.io/Digraphs
[Digraphs]: https://gap-packages.github.io/Digraphs
[RCWA package]: https://gap-packages.github.io/rcwa
[Semigroups package]: https://gap-packages.github.io/Semigroups
[Semigroups]: https://gap-packages.github.io/Semigroups
[Smallsemi]: https://gap-packages.github.io/smallsemi
[io]: https://gap-packages.github.io/io
[libsemigroups]: https://libsemigroups.github.io/libsemigroups
[orb package]: https://gap-packages.github.io/orb
[orb]: https://gap-packages.github.io/orb
[Bitbucket issue tracker]: https://bitbucket.org/james-d-mitchell/semigroups/issues
[images package]: https://gap-packages.github.io/images
[HPCombi]: libsemigroups.github.io/HPCombi/

