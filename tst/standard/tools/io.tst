#############################################################################
##
#W  standard/tools/io.tst
#Y  Copyright (C) 2016-2022                              James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local file, fname, gens, it, name, table, tables, x
gap> START_TEST("Semigroups package: standard/tools/io.tst");
gap> LoadPackage("semigroups", false);;

#
gap> SEMIGROUPS.StartTest();

# Test IteratorFromGeneratorsFile
gap> it := IteratorFromGeneratorsFile(Concatenation(SEMIGROUPS.PackageDir,
> "/non-existant-file.gz"));
fail
gap> it := IteratorFromGeneratorsFile(Concatenation(SEMIGROUPS.PackageDir,
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
Error, there should be 1 or 2 arguments
gap> ReadGenerators("non-existant-file");
Error, could not open the file non-existant-file
gap> file := IO_CompressedFile(name, "r");;
gap> ReadGenerators(file, 2);
[ <identity partial perm on [ 1, 3, 4 ]>, [1,2](3)(4) ]
gap> ReadGenerators(file, -1);
Error, the 2nd argument is not a positive integer
gap> ReadGenerators(file, 2000);
Error, the file only has 11 further entries
gap> ReadGenerators(3);
Error, the 1st argument is not a string or a file
gap> IO_Close(file);
true

# Test WriteGenerators
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/testdata");;
gap> gens := ReadGenerators(name);;
gap> name := Filename(DirectoryTemporary(), "testdata");;
gap> WriteGenerators(name, gens);
IO_OK
gap> WriteGenerators(name, gens, "w");
IO_OK
gap> WriteGenerators(name, gens, "w", 2);
Error, the 3rd or 4th argument is not a function
gap> WriteGenerators(name, gens, "x");
Error, the 3rd argument is not "a" or "w"
gap> file := IO_CompressedFile(name, "w");;
gap> WriteGenerators(file, gens);
IO_OK
gap> IO_Close(file);
true
gap> WriteGenerators(3, gens);
Error, the 1st argument is not a string or a file
gap> name := Filename(DirectoryTemporary(), "tmpfile");; 
gap> WriteGenerators(name, [FullTransformationMonoid(3)]);
IO_OK
gap> Length(ReadGenerators(name)[1]);
4
gap> WriteGenerators(name, gens, 1);
Error, the 3rd argument is not a string or a function
gap> WriteGenerators(1, 2, 3, 4, 5);
Error, there should be 2, 3, or 4 arguments
gap> WriteGenerators(name, [], "w");
Error, the 2nd argument is not a non-empty list

# Test ReadGenerators
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/trans3");;
gap> ReadGenerators(name);
[ [ Transformation( [ 1, 1, 1 ] ) ], [ Transformation( [ 1, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1 ] ) ], [ IdentityTransformation ], 
  [ Transformation( [ 1, 3, 2 ] ) ], [ Transformation( [ 2, 1, 1 ] ) ], 
  [ Transformation( [ 2, 3, 1 ] ) ] ]
gap> ReadGenerators(name, 4);
[ IdentityTransformation ]
gap> ReadGenerators(name, 2, 3);
Error, there should be 1 or 2 arguments
gap> ReadGenerators("non-existant-file");
Error, could not open the file non-existant-file
gap> file := IO_CompressedFile(name, "r");;
gap> ReadGenerators(file, 2);
[ Transformation( [ 1, 1, 2 ] ) ]
gap> ReadGenerators(file, -1);
Error, the 2nd argument is not a positive integer
gap> ReadGenerators(file, 2000);
Error, the file only has 5 further entries
gap> IO_Close(file);
true
gap> ReadGenerators(3);
Error, the 1st argument is not a string or a file
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/bipart4");;
gap> ReadGenerators(name);
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
gap> ReadGenerators(name);
[ [ [3,7][8,1,2,6,9][10,5] ] ]

# Test IteratorFromGeneratorsFile
gap> it := IteratorFromGeneratorsFile("non-existant-file.gz");
fail
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/trans3");;
gap> it := IteratorFromGeneratorsFile(name);
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

# Test ReadMultiplicationTable
# The following tests regularly fail in the CI cygwin jobs, and so are disabled
#@if not ARCH_IS_WINDOWS()
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/tables.gz");;
gap> tables := ReadMultiplicationTable(name);;
gap> Length(tables);
48
gap> ReadMultiplicationTable(name, 3);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 1 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 4, 5, 6, 7, 8, 9, 1, 3 ], [ 4, 4, 5, 6, 7, 8, 9, 1, 3, 4 ], 
  [ 5, 5, 6, 7, 8, 9, 1, 3, 4, 5 ], [ 6, 6, 7, 8, 9, 1, 3, 4, 5, 6 ], 
  [ 7, 7, 8, 9, 1, 3, 4, 5, 6, 7 ], [ 8, 8, 9, 1, 3, 4, 5, 6, 7, 8 ], 
  [ 9, 9, 1, 3, 4, 5, 6, 7, 8, 9 ], [ 1, 10, 3, 4, 5, 6, 7, 8, 9, 2 ] ]
gap> ReadMultiplicationTable(name, 3, 4);
Error, there should be 1 or 2 arguments
gap> ReadMultiplicationTable("non-existant-file");
Error, could not open the file "non-existant-file"
gap> file := IO_CompressedFile(name, "r");;
gap> ReadMultiplicationTable(file, 2);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 6, 7, 8, 1, 9, 10, 4, 5 ], [ 4, 4, 7, 8, 6, 9, 10, 1, 5, 3 ], 
  [ 5, 5, 8, 6, 7, 10, 1, 9, 3, 4 ], [ 6, 6, 1, 9, 10, 3, 4, 5, 7, 8 ], 
  [ 7, 7, 9, 10, 1, 4, 5, 3, 8, 6 ], [ 8, 8, 10, 1, 9, 5, 3, 4, 6, 7 ], 
  [ 9, 9, 4, 5, 3, 7, 8, 6, 10, 1 ], [ 10, 10, 5, 3, 4, 8, 6, 7, 1, 9 ] ]
gap> ReadGenerators(file, -1);
Error, the 2nd argument is not a positive integer
gap> ReadMultiplicationTable(file, -2);
Error, the 2nd argument is not a positive integer
gap> ReadMultiplicationTable(file, 100);
Error, the file only has 46 lines
gap> ReadMultiplicationTable(32);
Error, the 1st argument is not a string or a file
gap> IO_Close(file);
true
#@fi

# Test WriteMultiplicationTable
# The following tests regularly fail in the CI cygwin jobs, and so are disabled
#@if not ARCH_IS_WINDOWS()
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/tables.gz");;
gap> tables := ReadMultiplicationTable(name);;
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/tmptables");;
gap> WriteMultiplicationTable(name, tables);
IO_OK
gap> WriteMultiplicationTable(name, tables, "w");
IO_OK
gap> WriteMultiplicationTable(name, tables, "w", 2);
Error, there must be 2 or 3 arguments
gap> WriteMultiplicationTable(name, tables, "x");
Error, the 3rd argument is not "a" or "w"
gap> file := IO_CompressedFile(name, "w");;
gap> WriteMultiplicationTable(file, tables);
IO_OK
gap> IO_Close(file);
true
gap> WriteMultiplicationTable(3, tables);
Error, the 1st argument is not a string or a file
gap> name := Concatenation(SEMIGROUPS.PackageDir, "/data/tst/tmptables");;
gap> table := [[1, 2, 3, 4], [1, 2, 3, false], [2, 3, 4, 1], [4, 3, 2, 1]];;
gap> WriteMultiplicationTable(name, [table]);
Error, the 2nd argument is not a collection of rectangular tables containing o\
nly integers
gap> table := [[1, 2, 3], [1, 2], [3, 2, 1]];;
gap> WriteMultiplicationTable(name, [table]);
Error, the 2nd argument is not a collection of rectangular tables containing o\
nly integers
gap> table := [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
> [2, 4, 6, 7, 8, 9, 1, 11, 12, 13, 14, 3, 15, 5, 16, 10],
> [3, 6, 5, 9, 10, 8, 12, 13, 11, 1, 15, 14, 2, 16, 4, 7],
> [4, 7, 9, 1, 11, 12, 2, 14, 3, 15, 5, 6, 16, 8, 10, 13],
> [5, 8, 10, 11, 1, 13, 14, 2, 15, 3, 4, 16, 6, 7, 9, 12],
> [6, 9, 8, 12, 13, 11, 3, 15, 14, 2, 16, 5, 4, 10, 7, 1],
> [7, 1, 12, 2, 14, 3, 4, 5, 6, 16, 8, 9, 10, 11, 13, 15],
> [8, 11, 13, 14, 2, 15, 5, 4, 16, 6, 7, 10, 9, 1, 12, 3],
> [9, 12, 11, 3, 15, 14, 6, 16, 5, 4, 10, 8, 7, 13, 1, 2],
> [10, 13, 1, 15, 3, 2, 16, 6, 4, 5, 9, 7, 8, 12, 11, 14],
> [11, 14, 15, 5, 4, 16, 8, 7, 10, 9, 1, 13, 12, 2, 3, 6],
> [12, 3, 14, 6, 16, 5, 9, 10, 8, 7, 13, 11, 1, 15, 2, 4],
> [13, 15, 2, 16, 6, 4, 10, 9, 7, 8, 12, 1, 11, 3, 14, 5],
> [14, 5, 16, 8, 7, 10, 11, 1, 13, 12, 2, 15, 3, 4, 6, 9],
> [15, 16, 4, 10, 9, 7, 13, 12, 1, 11, 3, 2, 14, 6, 5, 8],
> [16, 10, 7, 13, 12, 1, 15, 3, 2, 14, 6, 4, 5, 9, 8, 11]];;
gap> WriteMultiplicationTable(name, [table]);
IO_OK
gap> WriteMultiplicationTable(name, [table + 250]);
Error, the 2nd argument is not a collection of rectangular tables with integer\
 entries from [1, 2, ..., n] (where n equals the number of rows of the table)
gap> table := MultiplicationTable(SmallGroup(257, 1));;
gap> WriteMultiplicationTable(name, [table]);
Error, the 2nd argument is not a collection of rectangular tables with at most\
 255 rows
gap> Exec("rm ", name);
#@fi

# Test IteratorFromMultiplicationTableFile
# The following tests regularly fail in the CI cygwin jobs, and so are disabled
#@if not ARCH_IS_WINDOWS()
gap> it := IteratorFromMultiplicationTableFile(Concatenation(SEMIGROUPS.PackageDir,
> "/non-existant-file.gz"));
fail
gap> it := IteratorFromMultiplicationTableFile(Concatenation(SEMIGROUPS.PackageDir,
> "/data/tst/tables.gz"));
<iterator>
gap> NextIterator(it);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], [ 4, 4, 5, 6, 7, 8, 9, 10, 1, 3 ], 
  [ 5, 5, 6, 7, 8, 9, 10, 1, 3, 4 ], [ 6, 6, 7, 8, 9, 10, 1, 3, 4, 5 ], 
  [ 7, 7, 8, 9, 10, 1, 3, 4, 5, 6 ], [ 8, 8, 9, 10, 1, 3, 4, 5, 6, 7 ], 
  [ 9, 9, 10, 1, 3, 4, 5, 6, 7, 8 ], [ 10, 10, 1, 3, 4, 5, 6, 7, 8, 9 ] ]
gap> NextIterator(it);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 6, 7, 8, 1, 9, 10, 4, 5 ], [ 4, 4, 7, 8, 6, 9, 10, 1, 5, 3 ], 
  [ 5, 5, 8, 6, 7, 10, 1, 9, 3, 4 ], [ 6, 6, 1, 9, 10, 3, 4, 5, 7, 8 ], 
  [ 7, 7, 9, 10, 1, 4, 5, 3, 8, 6 ], [ 8, 8, 10, 1, 9, 5, 3, 4, 6, 7 ], 
  [ 9, 9, 4, 5, 3, 7, 8, 6, 10, 1 ], [ 10, 10, 5, 3, 4, 8, 6, 7, 1, 9 ] ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true
gap> it := ShallowCopy(it);
<iterator>
gap> NextIterator(it);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 4, 5, 6, 7, 8, 9, 10, 1 ], [ 4, 4, 5, 6, 7, 8, 9, 10, 1, 3 ], 
  [ 5, 5, 6, 7, 8, 9, 10, 1, 3, 4 ], [ 6, 6, 7, 8, 9, 10, 1, 3, 4, 5 ], 
  [ 7, 7, 8, 9, 10, 1, 3, 4, 5, 6 ], [ 8, 8, 9, 10, 1, 3, 4, 5, 6, 7 ], 
  [ 9, 9, 10, 1, 3, 4, 5, 6, 7, 8 ], [ 10, 10, 1, 3, 4, 5, 6, 7, 8, 9 ] ]
gap> NextIterator(it);
[ [ 1, 1, 3, 4, 5, 6, 7, 8, 9, 10 ], [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ], 
  [ 3, 3, 6, 7, 8, 1, 9, 10, 4, 5 ], [ 4, 4, 7, 8, 6, 9, 10, 1, 5, 3 ], 
  [ 5, 5, 8, 6, 7, 10, 1, 9, 3, 4 ], [ 6, 6, 1, 9, 10, 3, 4, 5, 7, 8 ], 
  [ 7, 7, 9, 10, 1, 4, 5, 3, 8, 6 ], [ 8, 8, 10, 1, 9, 5, 3, 4, 6, 7 ], 
  [ 9, 9, 4, 5, 3, 7, 8, 6, 10, 1 ], [ 10, 10, 5, 3, 4, 8, 6, 7, 1, 9 ] ]
gap> IsDoneIterator(it);
false
gap> for x in it do od;
gap> IsDoneIterator(it);
true
#@fi

# Test read from an old format file
gap> ReadGenerators(Filename(DirectoriesPackageLibrary("semigroups",
>                                                      "data/tst/")[1], 
>                            "trans3-old"));
[ [ Transformation( [ 1, 1, 1 ] ) ], [ Transformation( [ 1, 1, 2 ] ) ], 
  [ Transformation( [ 1, 1 ] ) ], [ IdentityTransformation ], 
  [ Transformation( [ 1, 3, 2 ] ) ], [ Transformation( [ 2, 1, 1 ] ) ], 
  [ Transformation( [ 2, 3, 1 ] ) ] ]

# Test write using old format
gap> fname := Filename(DirectoryTemporary(), "tmpfile");;
gap> WriteGenerators(fname,
>                    [FullTransformationMonoid(3)],
>                    SEMIGROUPS.WriteGeneratorsLine);
IO_OK
gap> FullTransformationMonoid(3) = Semigroup(ReadGenerators(fname)[1]);
true
gap> WriteGenerators(fname,
>                    [[Bipartition([[1, 3, -3, -5, -6], [2, 4, 7], [5, 6, 8, 9, -8, -9],
>                                   [10, -1, -4], [-2, -7], [-10]])]],
>                    SEMIGROUPS.WriteGeneratorsLine);
IO_OK
gap> ReadGenerators(fname);
[ [ <bipartition: [ 1, 3, -3, -5, -6 ], [ 2, 4, 7 ], [ 5, 6, 8, 9, -8, -9 ], 
         [ 10, -1, -4 ], [ -2, -7 ], [ -10 ]> ] ]
gap> WriteGenerators(fname,
>                    [[PartialPerm([1, 2, 4, 5, 6, 7, 11, 12], 
>                                  [3, 5, 8, 12, 4, 1, 9, 7])]], 
>                    "a",
>                    SEMIGROUPS.WriteGeneratorsLine);
IO_OK
gap> ReadGenerators(fname);
[ [ <bipartition: [ 1, 3, -3, -5, -6 ], [ 2, 4, 7 ], [ 5, 6, 8, 9, -8, -9 ], 
         [ 10, -1, -4 ], [ -2, -7 ], [ -10 ]> ], 
  [ [2,5,12,7,1,3][6,4,8][11,9] ] ]
gap> WriteGenerators(fname, 
>                    [[Matrix(Integers, [[0]])]], 
>                    "w",
>                    SEMIGROUPS.WriteGeneratorsLine);
Error, the 2nd argument is incompatible with the file format

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: standard/tools/io.tst");
