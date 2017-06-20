#############################################################################
##
#W  standard/io.tst
#Y  Copyright (C) 2016                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: standard/io.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test IteratorFromPickledFile
gap> it := IteratorFromPickledFile(Concatenation(SEMIGROUPS.PackageDir, 
> "/non-existant-file.gz"));
fail
gap> it := IteratorFromPickledFile(Concatenation(SEMIGROUPS.PackageDir, 
> "/data/tst/testdata"));
<iterator>
gap> NextIterator(it);
[ <identity partial perm on [ 1, 2, 3 ]>, <identity partial perm on [ 1, 3 ]> 
 ]
gap> NextIterator(it);
[ <identity partial perm on [ 1, 3, 4 ]>, [1,2](3)(4) ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true
gap> it := ShallowCopy(it);
<iterator>
gap> NextIterator(it);
[ <identity partial perm on [ 1, 2, 3 ]>, <identity partial perm on [ 1, 3 ]> 
 ]
gap> NextIterator(it);
[ <identity partial perm on [ 1, 3, 4 ]>, [1,2](3)(4) ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true

# Test ReadGenerators
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/testdata");;
gap> gens := ReadGenerators(name);;
gap> Length(gens);
13
gap> ReadGenerators(name, 2);
[ <identity partial perm on [ 1, 3, 4 ]>, [1,2](3)(4) ]
gap> ReadGenerators(name, 2, 3);
Error, Semigroups: ReadGenerators: usage,
there should be 1 or 2 arguments,
gap> ReadGenerators("non-existant-file");
Error, Semigroups: ReadGenerators:
could not open the file non-existant-file,
gap> file := IO_CompressedFile(name, "r");;
gap> ReadGenerators(file, 2);
[ <identity partial perm on [ 1, 3, 4 ]>, [1,2](3)(4) ]
gap> ReadGenerators(file, -1);
Error, Semigroups: ReadGenerators: usage,
the second argument must be a positive integer,
gap> ReadGenerators(file, 2000);
Error, Semigroups: ReadGenerators:
the file only has 11 further entries,
gap> ReadGenerators(3);
Error, Semigroups: ReadGenerators: usage,
the first argument must be a string or a file,
gap> IO_Close(file);
true

# Test WriteGenerators
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/testdata");;
gap> gens := ReadGenerators(name);;
gap> WriteGenerators(name, gens);
IO_OK
gap> WriteGenerators(name, gens, "w");
IO_OK
gap> WriteGenerators(name, gens, "w", 2);
Error, Semigroups: WriteGenerators: usage,
there should be 2 or 3 arguments,
gap> WriteGenerators(name, gens, "x");
Error, Semigroups: WriteGenerators: usage,
the third argument must be "a" or "w",
gap> file := IO_CompressedFile(name, "w");;
gap> WriteGenerators(file, gens);
IO_OK
gap> IO_Close(file);
true
gap> WriteGenerators(3, gens);
Error, Semigroups: WriteGenerators: usage,
the first argument must be a string or a file,
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/tmpfile");;
gap> WriteGenerators(name, [FullTransformationMonoid(3)]);
IO_OK
gap> Length(ReadGenerators(name)[1]);
4
gap> Exec("rm ", name);

# Test ReadOldGenerators
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/trans3");;
gap> ReadOldGenerators(name);
[ [ Transformation( [ 1, 1, 1 ] ) ], [ Transformation( [ 1, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1 ] ) ], [ IdentityTransformation ], 
  [ Transformation( [ 1, 3, 2 ] ) ], [ Transformation( [ 2, 1, 1 ] ) ], 
  [ Transformation( [ 2, 3, 1 ] ) ] ]
gap> ReadOldGenerators(name, 4);
[ IdentityTransformation ]
gap> ReadOldGenerators(name, 2, 3);
Error, Semigroups: ReadOldGenerators: usage,
there should be 1 or 2 arguments,
gap> ReadOldGenerators("non-existant-file");
Error, Semigroups: ReadOldGenerators:
could not open the file non-existant-file,
gap> file := IO_CompressedFile(name, "r");;
gap> ReadOldGenerators(file, 2);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> ReadOldGenerators(file, -1);
Error, Semigroups: ReadOldGenerators: usage,
the second argument must be a positive integer,
gap> ReadOldGenerators(file, 2000);
Error, Semigroups: ReadOldGenerators:
the file only has 5 lines,
gap> IO_Close(file);
true
gap> ReadOldGenerators(3);
Error, Semigroups: ReadOldGenerators: usage,
the first argument must be a string or a file,
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/bipart4");;
gap> ReadOldGenerators(name);
[ [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1, -2, -3 ], [ -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1, -2 ], [ -3, -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1, -2 ], [ -3 ], [ -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1 ], [ -2, -3, -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1 ], [ -2, -3 ], [ -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4, -1 ], [ -2 ], [ -3 ], [ -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4 ], [ -1, -2, -3, -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4 ], [ -1, -2, -3 ], [ -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4 ], [ -1, -2 ], [ -3, -4 ]> ], 
  [ <block bijection: [ 1, 2, 3, 4, -1, -2, -3, -4 ]>, 
      <bipartition: [ 1, 2, 3, 4 ], [ -1, -2 ], [ -3 ], [ -4 ]> ] ]
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/pperm10");;
gap> ReadOldGenerators(name);
[ [ [3,7][8,1,2,6,9][10,5] ] ]

# Test IteratorFromOldGeneratorsFile
gap> it := IteratorFromOldGeneratorsFile("non-existant-file.gz");
fail
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/trans3");;
gap> it := IteratorFromOldGeneratorsFile(name);
<iterator>
gap> NextIterator(it);
[ Transformation( [ 1, 1, 1 ] ) ]
gap> NextIterator(it);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true
gap> it := ShallowCopy(it);
<iterator>
gap> NextIterator(it);
[ Transformation( [ 1, 1, 1 ] ) ]
gap> NextIterator(it);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/io.tst");
