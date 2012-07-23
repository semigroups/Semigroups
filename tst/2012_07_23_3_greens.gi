gap> 
gap> gens:=[ PartialPermNC( [ 1, 2, 3, 5 ], [ 5, 7, 3, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5 ], [ 6, 4, 1, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 7 ], [ 2, 7, 4, 5, 8 ] ),
>  PartialPermNC( [ 1, 2, 3, 5, 6 ], [ 5, 6, 1, 4, 3 ] ),
>  PartialPermNC( [ 1, 2, 4, 6, 7 ], [ 2, 1, 6, 7, 4 ] ),
>  PartialPermNC( [ 1, 3, 5, 6, 7 ], [ 6, 2, 3, 5, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 7 ], [ 4, 1, 6, 2, 8, 5 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 8 ], [ 5, 6, 3, 8, 2, 7 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 6, 7 ], [ 1, 5, 2, 6, 7, 4 ] ),
>  PartialPermNC( [ 1, 2, 3, 4, 5, 6, 8 ], [ 7, 5, 2, 8, 4, 1, 3 ] ) ];;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> Size(s);
72713
gap> NrRClasses(s);
25643
gap> NrDClasses(s);
4737
gap> NrLClasses(s);
11323
gap> NrIdempotents(s);
121
gap> IsRegularSemigroup(s);
false
gap> f:=Random(s);
[ 3, 4, 7 ] -> [ 4, 7, 8 ]
gap> d:=DClass(s, f);
{[ 3, 4, 7 ] -> [ 4, 7, 8 ]}
gap> Size(d);
282
gap> NrRClasses(d);
282
gap> NrLClasses(d);
1
gap> IsRegularDClass(d);
false
gap> RhoCosets(d);
<enumerator of perm group>
gap> Length(last);
6
gap> AsList(last2);
[ (), (4,7,8), (4,8,7), (7,8), (4,7), (4,8) ]
gap> SchutzenbergerGroup(d);
Group(())
gap> RhoOrbStabChain(d);
<stabilizer chain record, Base [ 8, 4 ], Orbit length 3, Size: 6>
gap> SemigroupDataSCC(d);
fail
gap> Position(DClasses(s), d);
18
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> SemigroupDataSCC(d);
[ 33, 35, 144, 146, 147, 148, 151, 152, 340, 341, 342, 343, 344, 345, 346, 
  353, 355, 356, 357, 358, 503, 519, 539, 540, 544, 553, 560, 561, 568, 571, 
  706, 707, 708, 709, 710, 711, 712, 713, 714, 715, 717, 718, 719, 720, 721, 
  724, 725, 726, 727, 728, 729, 730, 731, 1013, 1014, 1040, 1041, 1043, 1045, 
  1046, 1086, 1087, 1088, 1089, 1090, 1091, 1092, 1102, 1103, 1105, 1124, 
  1126, 1134, 1135, 1136, 1137, 1139, 1140, 1151, 1157, 1158, 1181, 1202, 
  1333, 1334, 1335, 1336, 1337, 1338, 1339, 1340, 1341, 1342, 1343, 1344, 
  1345, 1346, 1347, 1348, 1349, 1350, 1351, 1352, 1353, 1354, 1355, 1356, 
  1357, 1358, 1359, 1360, 1361, 1362, 1363, 1364, 1365, 1366, 1367, 1368, 
  1369, 1370, 1371, 1800, 1801, 1831, 1832, 1833, 1837, 1838, 1839, 1840, 
  1841, 1842, 1898, 1899, 1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 
  1908, 1909, 1910, 1911, 1913, 1918, 1928, 1929, 1932, 1933, 1958, 1959, 
  1960, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 2009, 2010, 2037, 2039, 
  2051, 2052, 2057, 2058, 2065, 2188, 2189, 2190, 2191, 2192, 2193, 2194, 
  2195, 2196, 2197, 2198, 2199, 2200, 2201, 2202, 2203, 2204, 2205, 2206, 
  2207, 2208, 2209, 2210, 2211, 2212, 2213, 2214, 2215, 2216, 2217, 2218, 
  2749, 2750, 2782, 2783, 2788, 2789, 2790, 2791, 2792, 2793, 2794, 2795, 
  2855, 2856, 2857, 2858, 2859, 2860, 2861, 2862, 2863, 2864, 2865, 2866, 
  2873, 2884, 2885, 2888, 2889, 2920, 2921, 2936, 2937, 2938, 2939, 2973, 
  2974, 2990, 2997, 3003, 3004, 3019, 3124, 3125, 3126, 3127, 3128, 3129, 
  3130, 3131, 3132, 3133, 3134, 3135, 3136, 3137, 3674, 3704, 3706, 3707, 
  3708, 3765, 3766, 3767, 3831, 3850, 3891, 3910, 3916, 3925, 4040, 4041, 
  4042, 4043, 4044, 4573, 4703, 4807, 5517 ]
gap> LambdaCosets(d);
[ () ]
gap> LambdaOrbSCC(d);
[ 49 ]
gap> RhoOrbSCC(d);
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
gap> ForAll(d, x-> x in d);
true
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> Length(enum);
282
gap> Size(d);
282
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], f ) ) called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 43 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> Length(lschutz);
1
brk> (Position(lscc[lm], ll)-1;
Syntax error: ) expected in *errin* line 3
(Position(lscc[lm], ll)-1;
                         ^
brk> Position(lscc[lm], ll)-1;
0
brk> Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1];
Syntax error: ) expected in *errin* line 5
Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1];
                                                          ^
brk>  Position(lschutz, LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], f))
> ;
fail
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> RhoOrbMults(ro, rm)[rl][1]*rep/cosets[j]^-1*f;
<empty mapping>
brk> (RhoOrbMults(ro, rm)[rl][1]*rep/cosets[j])^-1*f;
[ 4, 7, 8 ] -> [ 8, 4, 7 ]
brk> pp:=last;
Syntax error: warning: unbound global variable in *errin* line 14
pp:=last;
   ^
[ 4, 7, 8 ] -> [ 8, 4, 7 ]
brk> MappingPermListList(DomPP(pp), RanPP(pp));
(4,8,7)
brk> LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], f);
(4,8,7)
brk> lschutz;
[ () ]
brk> LambdaPerm(s)(RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j], RhoOrbMults(ro, rm)[rl][1]*g/cosets[j]);
(1,2,4,3,5,7)(6,8)
brk> g;
(4,8,7)
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 712
      p:=LambdaPerm(s)(rep, g);
        ^
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 715
        j:=PositionCanonical(cosets, p);
                                      ^
Syntax error: warning: unbound global variable in /Users/jdm/gap/pkg/citrus/ga\
p/greens.gi line 719
          if SiftedPermutation(schutz, p*cosets[j])=() then 
                                        ^
true
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 48 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> g;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk>  RhoOrbMults( ro, rm )[rl][1] * g / cosets[j];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> coset[j];
Syntax error: warning: unbound global variable in *errin* line 6
coset[j];
     ^
Error, Variable: 'coset' must have a value in
  <compiled or corrupted statement>  called from 
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 6 of *errin*
brk> quit;
gap> cosets[j];
Error, Variable: 'cosets' must have a value
not in any function at line 48 of *stdin*
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 49 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> cosets[j];
()
brk>  RhoOrbMults(ro, rm)[rl][1];
<identity on [ 1 .. 8 ]>
brk> quit
> ;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 53 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk>  Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f );
fail
brk> GreensRClasses( d )[(r + 1)];  f;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> r;
0
brk> f in GreensRClasses(d)[r+1];
false
brk> x;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
Error,  called from
enum!.ElementNumber( enum, nr ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 58 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
gap> Size(last);
1
gap> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> Size(last);
1
gap> Length(cosets);
Error, Variable: 'cosets' must have a value
not in any function at line 62 of *stdin*
gap> d;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> cosets:=RhoCosets(d);
<enumerator of perm group>
gap> Length(cosets);
6
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error,  called from
enum!.ElementNumber( enum, nr ) called from
<compiled or corrupted statement>  called from
func( elm ) called from
ForAllOp( C, func ) called from
<function "ForAll">( <arguments> )
 called from read-eval loop at line 66 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> f;
[ 3, 4, 7 ] -> [ 4, 7, 8 ]
brk> enum[2];
Error,  called from
enum!.ElementNumber( enum, nr ) called from
Error(  ); called from
enum!.ElementNumber( enum, nr ) called from
<compiled or corrupted statement>  called from
func( elm ) called from
...  at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d;
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 72 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> Length(lschutz)*(Position(lscc[lm], ll)-1);
0
brk> rl;
15
brk> Position(rscc[rm], rl);
1
brk> Length(cosets);
6
brk> j;
1
brk> enum!.m;
1
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> r;
0
brk> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> f in GreensRClasses(d)[2];
true
brk> RereadPackage("citrus/gap/greens.gi");
true
brk> quit;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[2]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 77 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
6
brk> rl;
15
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> r;
6
brk> RhoOrbMults(ro, rm)[rl][1]*
>        rep/cosets[j];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> f;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> lscc[lm][1];
49
brk> ll;
49
brk> rep;
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> g;
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> LambdaPerm(s)(rep, g);
(4,8,7)
brk> lschutz;
[ () ]
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[1]);
1
gap> Position(enum, enum[2]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 84 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
2
gap> Position(enum, enum[3]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 89 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> rl;
15
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> j;
1
brk> quit;
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> r;
Error, Variable: 'r' must have a value
not in any function at line 90 of *stdin*
gap> Position(enum, enum[3]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 91 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> j;
1
brk> RereadPackage("citrus/gap/greens.gi");
true
brk> quit;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 94 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> cosets[j];
()
brk> o;
Syntax error: warning: unbound global variable in *errin* line 3
o;
 ^
Error, Variable: 'o' must have a value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 3 of *errin*
brk> p;
(4,8,7)
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[2]);
Error, Function: number of arguments must be 2 (not 1) in
  schutz := LambdaOrbStabChain( d ); called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 98 of *stdin*
you can replace the argument list <args> via 'return <args>;'
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[3]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 102 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
2
brk> r;
Error, Variable: 'r' must have an assigned value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j;
2
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[3]);
3
gap> Position(enum, enum[4]);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Position( Enumerator( GreensRClasses( d )[(r + 1)] ), f )
  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 107 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> Size(d);
282
gap> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:+
Syntax error: ; expected
s:+
 ^
> ;
Syntax error: expression expected
;
^
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[4]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 119 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
Error, Variable: 'r' must have an assigned value in
  <compiled or corrupted statement>  called from 
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 1 of *errin*
you can 'return;' after assigning a value
brk_2> quit;
brk> j;
4
brk> (Position(rscc[rm], rl)-1)*Length(cosets)+j;
4
brk> GreensRClasses(d)[4];
{[ 1, 3, 6 ] -> [ 8, 4, 7 ]}
brk> enum[4];
[ 1, 3, 6 ] -> [ 8, 4, 7 ]
brk> quit;
gap> GreensRClasses(d)[3];
{[ 1, 3, 6 ] -> [ 8, 7, 4 ]}
gap> enum[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 126 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
1
brk> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
Error(  ); called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> r;
3
brk_2> quit;
brk> quit;
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 126 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
3
brk> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 130 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> quit;
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 130 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
2
brk> j;
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 134 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> f;
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> GreensRClasses(d)[1];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
brk> rep/cosets[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
brk> rep/cosets[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
brk> GreensRClasses(d)[2];
{[ 1, 3, 6 ] -> [ 4, 8, 7 ]}
brk> rep/cosets[3];
[ 1, 3, 6 ] -> [ 8, 7, 4 ]
brk> GreensRClasses(d)[3];
{[ 1, 3, 6 ] -> [ 8, 7, 4 ]}
brk> r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
0
brk> return;
Error, Function Calls: <func> must return a value in
  return enum!.NumberElement( enum, elm ); called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 134 of *stdin*
you can supply one by 'return <value>;'
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
Syntax error: warning: unbound global variable in *errin* line 1
enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
         ^
Error, Variable: 'r' must have a value in
  <compiled or corrupted statement>  called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 1 of *errin*
Syntax error: warning: unbound global variable in *errin* line 1
enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
                                                 ^
brk>  r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
   ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                  ^
Error, Variable: 'rscc' must have a value in
  <compiled or corrupted statement>  called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                     ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                          ^
Syntax error: warning: unbound global variable in *errin* line 2
 r:=(Position(rscc[rm], rl)-1)*Length(cosets)+j-1;
                                               ^
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s;=
<semigroup with 10 generators>
Syntax error: expression expected
s;=
  ^
> ;
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 140 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
1
brk> return;
Error, Function Calls: <func> must return a value in
  return enum!.NumberElement( enum, elm ); called from 
<function "unknown">( <arguments> )
 called from read-eval loop at line 140 of *stdin*
you can supply one by 'return <value>;'
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> return;
'return' cannot be used in this read-eval-print loop
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 141 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
2
brk> enum!.m*r+Position(Enumerator(GreensRClasses(d)[r+1]), f);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
Error(  ); called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 2 of *errin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk_2> quit;
brk> rscc;
[ [ 1 ], [ 2, 21, 26, 30, 31, 38, 46, 49, 50, 51, 52, 55, 57, 59, 60, 61, 94, 
      95, 99, 109, 110, 117, 129, 130, 139, 140, 142, 144, 147, 148, 150 ], 
  [ 3, 28 ], [ 4 ], [ 5 ], [ 6 ], [ 7 ], [ 8 ], [ 9 ], [ 10 ], [ 11 ], 
  [ 12, 13, 14, 16, 40, 44, 64, 68, 69, 70, 72, 73, 74, 75, 77, 81, 82, 88, 
      96, 97, 104, 112, 119, 122, 134, 135, 138 ], 
  [ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 
      80, 83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 
      107, 108, 113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ], 
  [ 24 ], [ 27 ], [ 35 ], [ 36 ], [ 37 ], [ 39, 111 ], [ 43 ], [ 48 ], 
  [ 53 ], [ 54 ], [ 56 ], [ 58 ], [ 62, 65, 66, 67, 71, 78, 100, 133 ], 
  [ 63 ], [ 92 ], [ 115, 116 ], [ 120 ], [ 124 ], [ 125 ], [ 131 ], [ 132 ], 
  [ 143 ], [ 145 ], [ 146 ], [ 149 ], [ 151 ] ]
brk> rscc[rm];
[ 15, 17, 18, 19, 20, 22, 23, 25, 29, 32, 33, 34, 41, 42, 45, 47, 76, 79, 80, 
  83, 84, 85, 86, 87, 89, 90, 91, 93, 98, 101, 102, 103, 105, 106, 107, 108, 
  113, 114, 118, 121, 123, 126, 127, 128, 136, 137, 141 ]
brk> rl;
15
brk> j;
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 145 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> r;
0
brk> j;
1
brk> return;
1
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 146 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
3
brk> cosets[3];
(4,8,7)
brk> p;
(4,8,7)
brk> cosets[3]*p;
(4,7,8)
brk> schutz;
false
brk> p;
(4,8,7)
brk> cosets;
<enumerator of perm group>
brk> PositionCanonical(cosets, p);
3
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> enum[1];
[ 1, 3, 6 ] -> [ 7, 4, 8 ]
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> Position(enum, enum[1]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 152 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
1
brk> return;
1
gap> Position(enum, enum[2]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 153 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> j;
2
brk> return;
2
gap> Position(enum, enum[3]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 154 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> return;
3
gap> Position(enum, enum[4]);
Error,  called from
enum!.NumberElement( enum, elm ) called from
<function "unknown">( <arguments> )
 called from read-eval loop at line 155 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> return;
4
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error,  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
<function "ForAll">( <arguments> )
 called from read-eval loop at line 156 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum[2];
[ 1, 3, 6 ] -> [ 4, 8, 7 ]
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> Length(enum);
282
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
Error, no method found! For debugging hints type ?Recovery from NoMethodFound
Error, no 1st choice method found for `+' on 2 arguments called from
enum!.m * r + Length( lschutz ) * (Position( lscc[lm], ll ) - 1) 
+ Position( lschutz, LambdaPerm( s )( RhoOrbMults( ro, rm )[rl][1] * rep 
      / cosets[j], RhoOrbMults( ro, rm )[rl][1] * g / cosets[j] ) )
  called from
enum!.NumberElement( enum, elm ) called from
Position( enum, x ) called from
func( elm ) called from
ForAllOp( C, func ) called from
...  at line 167 of *stdin*
you can 'quit;' to quit to outer loop, or
you can 'return;' to continue
brk> quit;
gap> RereadPackage("citrus/gap/greens.gi");
true
gap> s:=Semigroup(gens);
<semigroup with 10 generators>
gap> d:=DClasses(s)[18];
{[ 1, 3, 6 ] -> [ 7, 4, 8 ]}
gap> enum:=Enumerator(d);
<enumerator of D-class>
gap> ForAll(enum, x-> enum[Position(enum, x)]=x);
true
gap> NrDClasses(s);
4737
gap> for d in DClasses(s) do 
> enum:=Enumerator(d);
> ForAll(enum, x-> enum[Position(enum, x)]=x);
> od;
gap> for d in DClasses(s) do
> enum:=Enumerator(d);
> Print(ForAll(enum, x-> enum[Position(enum, x)]=x), "\n");
> od;
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
true
gap> Size(s);
72713
gap> NrRClasses(s);
25643
gap> NrLClasses(s);
11323
gap> NrDClasses(s);
4737
gap> NrIdempotents(s);
121
gap> quit;
