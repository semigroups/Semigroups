#############################################################################
##
#W  extreme/cong.tst
#Y  Copyright (C) 2014-15                                   Michael Torpey
##                                                          Wilf A. Wilson
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("Semigroups package: extreme/cong.tst");
gap> LoadPackage("semigroups", false);;

# Set info levels and user preferences
gap> SEMIGROUPS.StartTest();

# PairsCongTest1
gap> s := Semigroup([Transformation([1, 3, 4, 1, 3, 5]),
>    Transformation([2, 4, 6, 1, 6, 5]),
>    Transformation([4, 1, 2, 6, 2, 1]),
>    Transformation([4, 6, 4, 3, 3, 3]),
>    Transformation([5, 1, 6, 1, 6, 3]),
>    Transformation([5, 2, 5, 3, 5, 3])]);;
gap> gens := [
>  [Transformation([5, 5, 2, 4, 2, 4]),
>    Transformation([1, 5, 4, 5, 4, 5])],
>  [Transformation([3, 3, 3, 6, 3, 3]),
>    Transformation([1, 6, 6, 6, 6, 1])]];;
gap> cong := SemigroupCongruence(s, gens);
<semigroup congruence over <transformation semigroup of degree 6 with 6 
 generators> with 2 generating pairs>
gap> gens[2] in cong;
true
gap> x := Transformation([6, 5, 4, 4, 4, 6]);;
gap> y := Transformation([2, 2, 2, 6, 2, 4]);;
gap> z := Transformation([2, 4, 6, 1, 6, 5]);;
gap> [x, y] in cong; [x, z] in cong; [y, z] in cong;
true
false
false
gap> [x, y, z] in cong;
Error, Semigroups: \in (for a relation): usage,
the first arg <pair> must be a list of length 2,
gap> [x, Transformation([1])] in cong;
Error, Semigroups: \in (for a relation): usage,
elements of the first arg <pair> must be
in the range of the second arg <cong>,
gap> classes := CongruenceClasses(cong);;
gap> Size(classes) = NrCongruenceClasses(cong);
true
gap> classx := CongruenceClassOfElement(cong, x);;
gap> classy := CongruenceClassOfElement(cong, y);;
gap> classz := CongruenceClassOfElement(cong, z);
<congruence class of Transformation( [ 2, 4, 6, 1, 6, 5 ] )>
gap> classx = classy;
true
gap> classz = classx;
false
gap> x in classx;
true
gap> y in classx;
true
gap> x in classz;
false
gap> classx = classes[4];
true
gap> classz = classes[2];
true
gap> z * y in classz * classy;
true
gap> y * z in classx * classz;
true
gap> Size(classx);
3084
gap> q := s / cong;;
gap> P := [[(), 0, (1, 3), (1, 3), 0, (), 0],
>   [(), (1, 3), 0, 0, (1, 3), (), 0], [(), (1, 3), 0, (), 0, 0, ()],
>   [0, (), (1, 3), (1, 3), (), 0, 0], [0, 0, 0, (), (), (1, 3), ()],
>   [(), 0, (1, 3), 0, (), 0, ()]];;
gap> R := ReesZeroMatrixSemigroup(Group([(1, 3)]), P);;
gap> x := ReesZeroMatrixSemigroupElement(R, 1, (1, 3), 1);;
gap> y := ReesZeroMatrixSemigroupElement(R, 1, (), 1);;
gap> cong := SemigroupCongruenceByGeneratingPairs(R, [[x, y]]);;
gap> c := EquivalenceClasses(cong);;
gap> Size(c) = 43;
true
gap> cong := SemigroupCongruenceByGeneratingPairs(R, []);;
gap> c := EquivalenceClasses(cong);;
gap> Size(c) = 85;
true

# PairsCongTest3: \= for two semigroup congruences
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> s := Semigroup(Transformation([1]));;
gap> t := Monoid(gens);;
gap> u := UniversalSemigroupCongruence(s);
<universal semigroup congruence over <trivial transformation group of 
 degree 0 with 1 generator>>
gap> v := SemigroupCongruence(t, [gens[1], gens[1]]);
<semigroup congruence over <commutative non-regular transformation monoid of 
 degree 10 with 1 generator> with 0 generating pairs>
gap> NrCongruenceClasses(v);
6
gap> Size(t);
6
gap> u = v;
false
gap> u := UniversalSemigroupCongruence(t);
<universal semigroup congruence over <commutative non-regular transformation 
 monoid of size 6, degree 10 with 1 generator>>
gap> u = v;
false
gap> gens := List(t, x -> [gens[1], x]);;
gap> v := SemigroupCongruence(t, gens);
<semigroup congruence over <commutative non-regular transformation monoid 
 of size 6, degree 10 with 1 generator> with 5 generating pairs>
gap> u = v;
true
gap> NrCongruenceClasses(u);
1

# PairsCongTest4: \* for two semigroups congruence classes
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> s := Semigroup(gens);;
gap> gens := List(s, x -> [gens[1], x]);;
gap> u := SemigroupCongruence(s, gens);  # universal congruence
<semigroup congruence over <commutative non-regular transformation semigroup 
 of degree 10 with 1 generator> with 4 generating pairs>
gap> u = UniversalSemigroupCongruence(s);
true
gap> v := SemigroupCongruence(s, [gens[1], gens[1]]);  # trivial congruence
<semigroup congruence over <commutative non-regular transformation semigroup 
 of degree 10 with 1 generator> with 0 generating pairs>
gap> classes := Set(CongruenceClasses(v));
[ <congruence class of Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9, 1 ] )>, 
  <congruence class of Transformation( [ 2, 6, 6, 2, 6, 9, 9, 1, 1, 2 ] )>, 
  <congruence class of Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1, 5 ] )>, 
  <congruence class of Transformation( [ 6, 9, 9, 6, 9, 1, 1, 2, 2, 6 ] )>, 
  <congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )> ]
gap> ForAny(CongruenceClasses(u), x -> x in classes);
false
gap> classes[1] * CongruenceClasses(u)[1];
Error, Semigroups: \*: usage,
the args must be classes of the same congruence,
gap> CongruenceClasses(u)[1] * classes[1];
Error, Semigroups: \*: usage,
the args must be classes of the same congruence,
gap> classes[3] * classes[4];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> classes[4] * classes[3];
<congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6, 9 ] )>
gap> Representative(classes[5] * classes[2]) =
> Representative(classes[5]) * Representative(classes[2]);
true

# LatticeOfCongruences
gap> S := Semigroup([
> Transformation([1, 3, 4, 1]), Transformation([3, 1, 1, 3])]);;
gap> l := LatticeOfCongruences(S);
<poset of 52 congruences over <transformation semigroup of size 12, degree 4 
 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 7, 28, 26 ], [ 23, 10 ], [ 6 ], [ 17 ], [ 4, 33 ], [  ], [ 39, 27 ], 
  [ 9, 19, 5 ], [ 15, 4 ], [ 45 ], [ 34 ], [ 47, 44 ], [ 35, 48 ], 
  [ 17, 32 ], [ 42, 17 ], [ 18, 25, 50 ], [ 31 ], [ 38 ], [ 41, 33 ], 
  [ 4, 46 ], [ 36, 49 ], [ 37, 25 ], [ 30, 12, 45 ], [ 37, 21, 50 ], [ 38 ], 
  [ 27, 22, 24, 16 ], [ 37, 18 ], [ 39, 22 ], [ 42, 43 ], [ 9, 47, 20 ], 
  [ 6 ], [ 31 ], [ 14 ], [ 29, 40 ], [ 12, 52 ], [ 30, 8, 51 ], 
  [ 23, 36, 38 ], [ 35, 51 ], [ 2, 37 ], [ 42, 32 ], [ 15, 40, 14 ], 
  [ 3, 31 ], [ 31 ], [ 46 ], [ 20, 44 ], [ 17, 43 ], [ 15, 29, 46 ], 
  [ 52, 11 ], [ 51, 48 ], [ 38, 13, 49 ], [ 52, 19 ], [ 47, 41, 34 ] ]
gap> S := Semigroup([
> Transformation([1, 4, 3, 1, 4, 2]), Transformation([1, 6, 6, 3, 6, 6])]);;
gap> l := LatticeOfCongruences(S);
<poset of 5 congruences over <transformation semigroup of size 48, degree 6 
 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 2 ], [ 4 ], [ 5 ], [ 3 ], [  ] ]
gap> S := Semigroup([
> Transformation([4, 3, 1, 1, 6, 4]), Transformation([4, 3, 6, 4, 2, 3])]);;
gap> l := LatticeOfCongruences(S);
<poset of 328 congruences over <transformation semigroup of size 21, degree 6 
 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 2, 50 ], [ 8, 52, 40 ], [ 16, 72, 66 ], [ 74, 11 ], [ 77, 83, 36 ], 
  [ 89, 87 ], [ 94, 63, 86 ], [ 10, 99, 15 ], [ 3, 101, 108, 105 ], 
  [ 4, 116, 110 ], [ 43, 120, 73 ], [ 100, 129, 125 ], [ 109, 132 ], 
  [ 57, 37 ], [ 110, 13, 95, 144 ], [ 67, 149, 118 ], [ 58, 14 ], 
  [ 156, 154 ], [ 102, 159 ], [ 111, 130, 135, 80 ], [ 168, 119, 170, 167 ], 
  [ 21, 171, 172 ], [ 61, 175 ], [ 62, 22, 178 ], [ 112, 81, 20 ], [ 183 ], 
  [ 186, 187 ], [ 153 ], [ 27, 188 ], [ 153 ], [ 32, 189 ], [ 191, 190, 27 ], 
  [ 64 ], [ 65, 31 ], [ 153 ], [ 96, 195 ], [ 59 ], [ 107, 143, 194 ], 
  [ 98, 82, 136 ], [ 58, 210 ], [ 40, 17 ], [ 151, 83 ], [ 51, 199 ], 
  [ 16, 200, 216 ], [ 7, 218 ], [ 114, 131, 139, 78, 212 ], [ 3, 44, 220 ], 
  [ 115, 77, 46, 222 ], [ 48, 5, 42 ], [ 41 ], [ 16, 202, 214 ], 
  [ 99, 49, 210 ], [ 69, 145, 180 ], [ 75, 221, 146 ], [ 71, 185 ], 
  [ 117, 222, 147 ], [ 95, 140, 59 ], [ 15, 57, 152 ], [ 97, 203 ], 
  [ 18, 225, 223 ], [ 18, 226, 173 ], [ 21, 68, 227, 176 ], [ 71, 182 ], 
  [ 35, 28, 30 ], [ 32, 192, 29 ], [ 118, 231 ], [ 7, 234, 229 ], 
  [ 119, 236, 230 ], [ 121, 177 ], [ 148, 227, 69 ], [ 18, 228, 196 ], 
  [ 149, 62, 70, 231 ], [ 199, 237, 6 ], [ 47, 120, 76 ], [ 219, 122 ], 
  [ 220, 123, 75 ], [ 9, 78, 85, 96 ], [ 101, 12, 98, 242, 142 ], 
  [ 78, 39, 150 ], [ 103, 124, 164, 143, 162 ], [ 104, 80, 38, 179 ], 
  [ 240, 193 ], [ 79, 195 ], [ 70, 241, 245, 53 ], [ 108, 242, 81, 197 ], 
  [ 248, 182 ], [ 88, 26 ], [ 7, 250, 183 ], [ 45, 88, 92 ], [ 174, 184 ], 
  [ 217, 249 ], [ 218, 250, 91 ], [ 226, 228, 90 ], [ 61, 71, 93, 248 ], 
  [ 109, 252, 97 ], [ 105, 142, 197 ], [ 113, 254 ], [ 100, 240, 209, 251 ], 
  [ 116, 48, 144, 56 ], [ 16, 106, 258, 256 ], [ 100, 261, 259 ], 
  [ 60, 161, 262 ], [ 255, 260, 244, 263 ], [ 62, 103, 107, 245, 265 ], 
  [ 66, 259, 268 ], [ 67, 269, 266 ], [ 68, 260, 208, 267 ], 
  [ 72, 261, 104, 84, 268 ], [ 11, 272, 113 ], [ 109, 274 ], 
  [ 270, 103, 166 ], [ 24, 104, 111, 181 ], [ 73, 278 ], [ 271, 101, 280 ], 
  [ 47, 9, 114, 281 ], [ 74, 115, 274, 117 ], [ 76, 281, 275, 54 ], 
  [ 229, 283 ], [ 60, 285, 282 ], [ 51, 44, 237, 123 ], [ 232, 169 ], 
  [ 213, 215, 238 ], [ 214, 216, 239, 122 ], [ 255, 126, 286 ], [ 256, 290 ], 
  [ 257, 289 ], [ 121, 204, 287 ], [ 148, 246, 288, 127 ], [ 258, 124, 290 ], 
  [ 270, 124 ], [ 271, 12, 295 ], [ 272, 131, 134 ], [ 122, 294, 205 ], 
  [ 273, 295 ], [ 270, 164, 163 ], [ 251, 193 ], [ 257, 19, 297 ], 
  [ 139, 39, 198 ], [ 271, 98, 201, 299 ], [ 252, 138, 203 ], 
  [ 273, 299, 206 ], [ 259, 125, 251, 301 ], [ 260, 126, 137, 300 ], 
  [ 274, 132, 252, 46, 147 ], [ 127, 165 ], [ 133, 211 ], 
  [ 275, 134, 141, 212 ], [ 233, 170, 121 ], [ 234, 21, 148, 283 ], 
  [ 142, 136 ], [ 46, 138, 79 ], [ 144, 140, 151 ], [  ], [ 155 ], [ 153 ], 
  [ 35, 155 ], [ 153 ], [ 35, 157 ], [ 262 ], [ 224 ], [ 225, 160 ], 
  [ 263, 286, 296, 300 ], [ 276, 243 ], [ 255, 243, 137, 296 ], [ 287, 302 ], 
  [ 293, 244 ], [ 307, 282, 169 ], [ 61, 60, 309, 307 ], [ 308, 284, 27 ], 
  [ 309, 285, 32, 169 ], [ 168, 23, 310 ], [ 170, 310, 31 ], [ 154, 174 ], 
  [ 155, 28 ], [ 226, 33 ], [ 167, 230, 177 ], [ 169, 235, 29 ], 
  [ 227, 172, 34 ], [ 265, 162, 194 ], [ 177, 165, 207 ], [ 178, 245, 166 ], 
  [ 196 ], [ 63, 55 ], [ 155 ], [ 228 ], [ 28, 157 ], [ 157 ], [ 187 ], 
  [ 191, 33 ], [ 158, 187 ], [ 64, 158, 186 ], [ 190, 188 ], [ 253 ], 
  [ 267, 300 ], [ 150 ], [ 154, 184 ], [ 268, 301, 179 ], [ 201, 82 ], 
  [ 202, 87 ], [ 67, 45, 317 ], [ 277, 240, 313 ], [ 67, 88, 315 ], 
  [ 254, 198 ], [ 232, 305 ], [ 238, 312 ], [ 279, 313 ], [ 235, 302 ], 
  [ 236, 303, 207 ], [ 258, 247, 164, 298 ], [ 152, 42 ], [ 294, 241, 166 ], 
  [ 280, 295, 299, 242, 20 ], [ 148, 314 ], [ 149, 315, 213 ], 
  [ 148, 316, 172 ], [ 149, 317, 22, 215 ], [ 93, 175 ], [ 94, 23, 217 ], 
  [ 70, 215, 178 ], [ 72, 216, 24, 219 ], [ 219, 84, 211, 181 ], 
  [ 281, 85, 212, 25 ], [ 154, 224 ], [ 155, 157 ], [ 156, 158, 224 ], 
  [ 156, 64, 174 ], [ 170, 236, 65, 177 ], [ 156, 184 ], [ 86, 318 ], 
  [ 282, 235 ], [ 283, 176, 69 ], [ 90, 308 ], [ 93, 309, 232 ], 
  [ 94, 168, 233, 318 ], [ 284, 188 ], [ 285, 192, 235 ], 
  [ 202, 200, 89, 239 ], [ 314, 316, 91 ], [ 315, 317, 92, 238 ], 
  [ 106, 247, 253 ], [ 128, 244, 145 ], [ 261, 129, 209, 80, 301 ], 
  [ 264, 19, 304 ], [ 288, 303, 165 ], [ 227, 244, 208, 180 ], 
  [ 233, 319, 204 ], [ 269, 243, 311 ], [ 173, 196, 90 ], [ 93, 185 ], 
  [ 94, 55, 249 ], [ 256, 253, 298 ], [ 272, 139, 254, 141 ], [ 266, 311 ], 
  [ 278, 201, 206 ], [ 21, 264, 257, 288, 320 ], [ 118, 266, 322 ], 
  [ 119, 102, 292, 321 ], [ 149, 269, 255, 128, 322 ], [ 256, 324 ], 
  [ 257, 303, 323 ], [ 258, 103, 241, 324 ], [ 223, 160 ], [ 320, 323, 165 ], 
  [ 168, 102, 319, 325 ], [ 176, 263, 267, 180 ], [ 229, 326 ], 
  [ 230, 323, 207 ], [ 231, 324, 265, 53 ], [ 234, 264, 246, 326 ], 
  [ 22, 255, 276, 293 ], [ 44, 100, 277, 327 ], [ 120, 271, 278, 273 ], 
  [ 123, 327, 279, 133 ], [ 272, 114, 275 ], [ 273, 280, 146 ], 
  [ 171, 264, 306 ], [ 200, 106, 328 ], [ 237, 277, 279 ], [ 239, 328, 205 ], 
  [ 327, 261, 111, 211 ], [ 220, 108, 280, 112, 221 ], [ 223, 284 ], 
  [ 318, 167, 121 ], [ 224, 187 ], [ 225, 190, 284 ], [ 320, 289 ], 
  [ 169, 305, 291 ], [ 170, 319, 292, 287 ], [ 321 ], [ 322, 286 ], 
  [ 284, 160 ], [ 285, 161, 291 ], [ 172, 288, 306 ], [ 215, 128, 312, 293 ], 
  [ 327, 129, 130 ], [ 320, 304, 297 ], [ 321, 159 ], [ 322, 311, 296 ], 
  [ 327, 209, 313, 135 ], [ 323, 289, 297 ], [ 324, 290, 298, 162 ], [ 291 ], 
  [ 292, 302 ], [ 325, 159 ], [ 308, 160 ], [ 310, 319 ], [ 173, 223, 308 ], 
  [ 174, 224, 186 ], [ 226, 225, 191, 308 ], [ 309, 175, 189 ], [ 326, 304 ], 
  [ 316, 246, 306 ], [ 328, 247, 163 ], [ 233, 249 ], [ 234, 250, 314 ], 
  [ 233, 217, 310 ], [ 234, 218, 171, 316 ], [ 248, 307, 232 ], 
  [ 309, 161, 305 ], [ 167, 325, 321, 287 ], [ 282, 262, 291 ], 
  [ 283, 326, 320, 127 ], [ 321, 302 ], [ 322, 263, 145 ], [ 307, 262, 305 ], 
  [ 318, 325, 204 ], [ 216, 258, 328, 270, 294 ], [ 317, 269, 276, 312 ] ]
gap> S := Semigroup([
> Transformation([1, 5, 4, 5, 2]), Transformation([4, 5, 1, 3, 5])]);;
gap> l := LatticeOfCongruences(S);
<poset of 207 congruences over <transformation semigroup of size 74, degree 5 
 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 32, 55 ], [ 48, 11 ], [ 28, 60, 62 ], [ 12, 66, 21 ], [ 70, 73 ], 
  [ 85, 78, 19 ], [ 83, 57 ], [ 87, 17 ], [ 93 ], [ 114, 109, 58 ], 
  [ 113, 56 ], [ 65, 116, 107 ], [ 118, 9 ], [ 88, 127 ], [ 22 ], 
  [ 14, 128 ], [ 129 ], [ 131 ], [ 140, 136 ], [ 17, 142 ], [ 107, 143 ], 
  [  ], [ 144, 148, 89 ], [ 150, 90 ], [ 152 ], [ 160 ], [ 40, 41, 31 ], 
  [ 61, 30 ], [ 79, 145, 133 ], [ 144, 110, 92 ], [ 117, 42, 156, 157 ], 
  [ 3, 27, 39, 43 ], [ 59, 161, 154 ], [ 166, 38 ], [ 162, 70, 37 ], 
  [ 147, 163, 71, 135 ], [ 146, 164, 72, 96 ], [ 120, 167, 170 ], 
  [ 60, 34, 171 ], [ 28, 172, 156 ], [ 172, 34, 157 ], [ 122, 173 ], 
  [ 62, 153, 52, 171 ], [ 66, 111 ], [ 4, 112 ], [ 51 ], [ 168, 102 ], 
  [ 4, 113, 44 ], [ 17, 138 ], [ 12, 104 ], [ 17 ], [ 63, 158, 53 ], 
  [ 64, 159 ], [ 43, 160 ], [ 54, 26 ], [ 103, 7 ], [ 49 ], [ 177, 175, 6 ], 
  [ 162, 108 ], [ 35, 5, 179 ], [ 35, 59, 110 ], [ 10, 63, 179 ], 
  [ 114, 64 ], [ 115, 74 ], [ 8, 180, 20 ], [ 116, 16, 143 ], [ 181, 18 ], 
  [ 183, 67 ], [ 184, 68 ], [ 45, 72 ], [ 151, 81, 130 ], [ 150, 112, 182 ], 
  [ 69, 74 ], [ 184 ], [ 145, 185, 132 ], [ 151, 25 ], [ 163, 82, 134 ], 
  [ 86, 67, 136 ], [ 36, 186, 77, 137 ], [ 87, 138 ], [ 84, 46 ], 
  [ 169, 139 ], [ 8, 80, 49 ], [ 8, 51 ], [ 29, 86, 75, 140 ], 
  [ 36, 185, 181, 141 ], [ 15, 129 ], [ 15, 126 ], [ 187, 189, 75 ], 
  [ 191, 76 ], [ 98, 99 ], [ 187, 176, 29 ], [ 58, 105, 97 ], [ 174, 192 ], 
  [ 194 ], [ 188, 193, 182, 36 ], [ 175, 106 ], [ 92, 196 ], [ 196, 95 ], 
  [ 180, 80 ], [ 104, 81 ], [ 195, 82 ], [ 65, 83, 100 ], [ 65, 84 ], 
  [ 177, 91, 106 ], [ 178, 95 ], [ 20, 197 ], [ 47, 164, 174 ], 
  [ 115, 69, 175 ], [ 149, 108, 37, 176 ], [ 116, 100 ], [ 50, 101 ], 
  [ 12, 103, 111 ], [ 23, 30, 115, 177 ], [ 148, 37, 184, 178 ], 
  [ 180, 14, 197 ], [ 122, 123, 91 ], [ 10, 124, 121, 93 ], [ 108, 198, 94 ], 
  [ 199, 95 ], [ 109, 125, 97 ], [ 30, 200, 98 ], [ 200, 120, 99 ], 
  [ 114, 117, 125, 105 ], [ 115, 120, 106 ], [ 22 ], [ 126 ], [ 127 ], 
  [ 22 ], [ 152, 46 ], [ 25, 130 ], [ 201, 203 ], [ 201, 137 ], [ 139, 205 ], 
  [ 202, 205, 130 ], [ 141, 18 ], [ 204, 134, 135 ], [ 129 ], [ 206 ], 
  [ 132, 133, 141 ], [ 203, 135, 131 ], [ 129, 126 ], [ 197, 128 ], 
  [ 149, 187 ], [ 186, 201 ], [ 168, 150, 188 ], [ 169, 151, 202 ], 
  [ 146, 24, 189 ], [ 47, 146, 190 ], [ 50, 191 ], [ 84, 152 ], [ 51 ], 
  [ 118, 158, 155 ], [ 119, 165 ], [ 121, 159 ], [ 122, 207 ], 
  [ 123, 173, 207, 38 ], [ 124, 31, 159 ], [ 125, 38 ], [ 13, 153 ], 
  [ 162, 165 ], [ 2, 45, 164 ], [ 169, 81, 205 ], [ 168, 112, 193 ], [ 198 ], 
  [ 35, 161, 170 ], [ 199 ], [ 11, 50, 195 ], [ 7, 84, 206 ], [ 199, 165 ], 
  [ 179, 155, 53 ], [ 61, 166, 33, 207 ], [ 200, 167 ], [ 102, 193, 77 ], 
  [ 178, 68, 78 ], [ 190, 174, 96, 79 ], [ 89, 92, 178, 85 ], 
  [ 189, 96, 183, 86 ], [ 109, 64, 73 ], [ 87, 88, 142 ], [ 76, 71, 131 ], 
  [ 191, 101, 71 ], [ 90, 182, 181 ], [ 24, 72, 183 ], [ 147, 76, 203 ], 
  [ 147, 82, 204 ], [ 190, 145 ], [ 195, 191, 147 ], [ 188, 90, 185 ], 
  [ 102, 188, 186 ], [ 104, 151 ], [ 193 ], [ 195, 101, 163 ], [ 96, 192 ], 
  [ 56, 104, 169 ], [ 176, 94, 194 ], [ 142, 127 ], [ 164, 192 ], 
  [ 37, 198, 194 ], [ 110, 119, 199, 196 ], [ 204 ], [ 206, 152 ], 
  [ 202, 25 ], [ 139, 202 ], [ 206, 46 ], [ 57, 51 ], [ 200, 154, 170 ] ]
gap> S := Semigroup([
> Transformation([1, 3, 5, 5, 3]), Transformation([4, 5, 5, 5, 2])]);;
gap> l := LatticeOfCongruences(S);
<poset of 225 congruences over <transformation semigroup of size 18, degree 5 
 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 45, 32, 36 ], [ 48, 62 ], [ 65, 64 ], [ 10, 67, 14, 69 ], 
  [ 24, 73, 72, 80 ], [ 25 ], [ 11, 85, 15, 30 ], [ 72, 12, 92, 93, 13 ], 
  [ 104, 107 ], [ 71, 111, 113 ], [ 86, 116, 102 ], [ 74, 95, 119, 120 ], 
  [ 75, 17, 125, 126, 88 ], [ 40, 127, 128 ], [ 41, 129, 122 ], 
  [ 13, 130, 19 ], [ 76, 135, 136, 96 ], [ 22, 137, 68 ], [ 88, 38, 27 ], 
  [ 97, 81, 77 ], [ 83, 26 ], [ 52, 140, 112 ], [ 98, 18, 141 ], 
  [ 99, 89, 143 ], [  ], [ 28 ], [ 90, 147 ], [ 56 ], [ 101, 26 ], 
  [ 102, 31, 122 ], [ 103, 123, 29 ], [ 8, 151, 16 ], [ 5, 104, 8, 37, 152 ], 
  [ 78, 105, 91 ], [ 79, 106 ], [ 157, 151 ], [ 80, 108, 93, 34 ], 
  [ 94, 147 ], [ 81, 110, 138, 148 ], [ 144, 153 ], [ 145, 132 ], 
  [ 82, 95, 156, 158 ], [ 55, 22, 155 ], [ 58, 28 ], [ 33, 9, 157 ], 
  [ 50, 55, 160 ], [ 51, 162, 59 ], [ 164, 49 ], [ 167, 53 ], [ 52, 168 ], 
  [ 54, 165 ], [ 3, 169, 159 ], [ 63, 57 ], [ 6, 56 ], [ 52, 171 ], [ 25 ], 
  [ 172 ], [ 56 ], [ 165, 58 ], [ 163 ], [ 161 ], [ 164, 60 ], 
  [ 52, 170, 172 ], [ 66 ], [ 6, 66 ], [ 25 ], [ 111, 127, 23, 173 ], 
  [ 112, 70 ], [ 113, 173, 128, 7 ], [ 114, 26 ], [ 174, 40, 175 ], 
  [ 89, 74, 177, 75 ], [ 99, 82, 74, 182 ], [ 117, 178, 185 ], 
  [ 121, 76, 187 ], [ 131, 189 ], [ 179, 190 ], [ 142, 180, 176 ], 
  [ 181, 70 ], [ 143, 182, 177, 78 ], [ 183, 18, 139, 190 ], 
  [ 146, 178, 192 ], [ 54, 28 ], [ 115, 21, 29 ], [ 116, 129, 84, 31 ], 
  [ 193, 41, 149 ], [ 162, 83, 44 ], [ 20, 96, 39, 94, 90 ], 
  [ 117, 195, 121 ], [ 77, 100, 148, 197 ], [ 176, 118, 124 ], 
  [ 177, 119, 125 ], [ 177, 120, 126, 91 ], [ 81, 109, 197 ], 
  [ 178, 199, 200, 17 ], [ 97, 109, 110, 100 ], [ 53, 183, 179 ], 
  [ 43, 203 ], [ 146, 117, 205 ], [ 179, 206, 207 ], [ 44 ], [ 149, 103 ], 
  [ 150, 101 ], [ 73, 42, 12, 108, 208 ], [ 180, 154, 118 ], [ 181 ], 
  [ 208 ], [ 182, 158, 120, 105 ], [ 183, 206 ], [ 183, 201, 207 ], 
  [ 174, 98, 209 ], [ 159, 114 ], [ 175, 209, 11 ], [ 161, 28 ], [ 87, 101 ], 
  [ 193, 115, 103 ], [ 196, 211 ], [ 184, 198 ], [ 185, 199 ], 
  [ 185, 200, 118 ], [ 131, 213, 20 ], [ 132, 123 ], [ 133, 26 ], 
  [ 186, 134, 138 ], [ 187, 135, 94 ], [ 187, 136, 39, 124 ], 
  [ 144, 18, 214 ], [ 153, 214, 15 ], [ 145, 21, 123 ], [ 125, 38 ], 
  [ 49, 216, 97 ], [ 133 ], [ 165, 28 ], [ 188, 201 ], [ 189, 109 ], 
  [ 189, 110, 134 ], [ 140, 21, 70 ], [ 139, 201, 35 ], [ 202, 137, 79 ], 
  [ 169, 83, 114 ], [ 203, 137, 84 ], [ 204, 194 ], [ 205, 195, 142 ], 
  [ 50, 22, 217 ], [ 51, 83, 133 ], [ 2, 196, 219 ], [ 197 ], 
  [ 190, 207, 35 ], [ 150, 132 ], [ 59, 133, 44 ], [ 92, 130 ], 
  [ 80, 208, 92 ], [ 217, 41 ], [ 191, 198 ], [ 171, 140, 87 ], [ 192, 199 ], 
  [ 152, 107 ], [ 192, 200, 154 ], [ 64, 161 ], [ 168, 171, 47 ], [ 66, 56 ], 
  [ 54, 58 ], [ 160, 166 ], [ 46, 167, 163 ], [ 56 ], [ 168, 170 ], 
  [ 50, 63, 166 ], [ 169, 51 ], [ 65, 54, 161 ], [ 169, 61 ], [ 169, 162 ], 
  [ 159, 61 ], [ 209, 214, 141, 85 ], [ 46, 144, 43, 220 ], [ 220, 153, 86 ], 
  [ 194, 184, 69, 186 ], [ 195, 185, 4, 187, 176 ], [ 196, 222, 76 ], 
  [ 57, 223 ], [ 204, 191, 184 ], [ 61, 114 ], [ 205, 192, 185, 180 ], 
  [ 63, 22, 202, 223 ], [ 210, 221, 113 ], [ 211, 222, 10, 184 ], 
  [ 212, 188, 128 ], [ 213, 189, 14, 186 ], [ 215, 153 ], [ 216, 40, 188 ], 
  [ 223, 68, 79 ], [ 218, 221 ], [ 219, 222, 191 ], [ 47, 145, 87, 150 ], 
  [ 210, 173, 212 ], [ 211, 67, 213, 194 ], [ 48, 225, 131 ], [ 190, 206 ], 
  [ 221, 134 ], [ 222, 135 ], [ 222, 136, 198 ], [ 202, 106 ], 
  [ 170, 140, 181 ], [ 155, 115 ], [ 218, 210 ], [ 219, 211, 204 ], [ 223 ], 
  [ 223, 106 ], [ 182, 156, 119 ], [ 220, 203, 116 ], [ 224, 209 ], 
  [ 225, 111, 210 ], [ 215, 214, 139 ], [ 216, 127, 81, 212 ], 
  [ 217, 137, 129 ], [ 166, 217, 202 ], [ 167, 144, 183, 215 ], 
  [ 168, 140, 145 ], [ 60, 224 ], [ 62, 225, 218 ], [ 160, 217, 155, 193 ], 
  [ 224, 175, 188 ], [ 225, 71, 189, 221 ], [ 172, 112, 181 ], 
  [ 163, 220, 215 ], [ 164, 174, 216, 224 ] ]
gap> S := OrderEndomorphisms(2);;
gap> l := LatticeOfCongruences(S);
<poset of 3 congruences over <regular transformation monoid of size 3, 
 degree 2 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 3 ], [  ], [ 2 ] ]
gap> S := OrderEndomorphisms(3);;
gap> l := LatticeOfCongruences(S);
<poset of 4 congruences over <regular transformation monoid of size 10, 
 degree 3 with 3 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 4 ], [ 3 ], [  ], [ 2 ] ]
gap> S := OrderEndomorphisms(4);;
gap> l := LatticeOfCongruences(S);
<poset of 5 congruences over <regular transformation monoid of size 35, 
 degree 4 with 4 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 5 ], [ 3 ], [ 4 ], [  ], [ 2 ] ]
gap> S := PartitionMonoid(2);;
gap> l := LatticeOfCongruences(S);
<poset of 13 congruences over <regular bipartition *-monoid of size 15, 
 degree 2 with 3 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 3, 4, 9 ], [ 6, 11 ], [ 2, 5, 8 ], [ 2, 10 ], [ 6, 12 ], [ 13 ], [  ], 
  [ 11, 12 ], [ 5, 10 ], [ 6 ], [ 13 ], [ 13 ], [ 7 ] ]

# Check robustness against non-free infinite semigroups
#TODO!

# SEMIGROUPS_UnbindVariables
gap> Unbind(P);
gap> Unbind(R);
gap> Unbind(S);
gap> Unbind(c);
gap> Unbind(classes);
gap> Unbind(classx);
gap> Unbind(classy);
gap> Unbind(classz);
gap> Unbind(cong);
gap> Unbind(gens);
gap> Unbind(l);
gap> Unbind(q);
gap> Unbind(s);
gap> Unbind(t);
gap> Unbind(u);
gap> Unbind(v);
gap> Unbind(x);
gap> Unbind(y);
gap> Unbind(z);

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/cong.tst");
