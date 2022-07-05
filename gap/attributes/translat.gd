############################################################################
##
# W  translat.gd
# Y  Copyright (C) 2015-22                      James D. Mitchell, Finn Smith
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#! @BeginGroup IsXTranslation
#! @GroupTitle IsXTranslation
#! @Description
#! All, and only, left [right] translations belong to <C>IsLeftTranslation</C> 
#! [<C>IsRightTranslation</C>]. These are both subcategories of 
#! <C>IsSemigroupTranslation</C>, which itself is a subcategory of
#! <C>IsAssociativeElement</C>.
#! @BeginExampleSession
#! gap> S := RectangularBand(3, 4);;
#! gap> l := Representative(LeftTranslations(S));
#! <left translation on <regular transformation semigroup of size 12, 
#!  degree 8 with 4 generators>>
#! gap> IsSemigroupTranslation(l);
#! true
#! gap> IsLeftTranslation(l);
#! true
#! gap> IsRightTranslation(l);
#! false
#! @EndExampleSession
DeclareCategory("IsSemigroupTranslation",
                IsAssociativeElement and IsMultiplicativeElementWithOne);
DeclareCategory("IsLeftTranslation",
                IsSemigroupTranslation);
DeclareCategory("IsRightTranslation",
                IsSemigroupTranslation);
#! @EndGroup

#! @Description
#! All, and only, bitranslations belong to <C>IsBitranslation</C>. This is a
#! subcategory of <Ref Filt="IsAssociativeElement" BookName="ref"/>.
#! @BeginExampleSession
#! gap> G := Group((1, 2), (3, 4));;
#! gap> A := AsList(G);;
#! gap> mat := [[A[1], 0, A[1]],
#! > [A[2], A[2], A[4]]];;
#! gap> S := ReesZeroMatrixSemigroup(G, mat);;
#! gap> L := LeftTranslations(S);;
#! gap> R := RightTranslations(S);;
#! gap> l := OneOp(Representative(L));;
#! gap> r := OneOp(Representative(R));;
#! gap> H := TranslationalHull(S);;
#! gap> x := Bitranslation(H, l, r);;
#! gap> IsBitranslation(x);
#! true
#! gap> IsSemigroupTranslation(x);
#! false
#! @EndExampleSession
DeclareCategory("IsBitranslation",
                IsAssociativeElement and IsMultiplicativeElementWithOne);

#! @BeginGroup IsXTranslationCollection
#! @GroupTitle IsXTranslationCollection
#! @Description
#! Every collection of left-, right-, or bi-translations belongs to the 
#! respective category <Ref Filt="IsXTranslationCollection"/>.
DeclareCategoryCollections("IsSemigroupTranslation");
DeclareCategoryCollections("IsLeftTranslation");
DeclareCategoryCollections("IsRightTranslation");
DeclareCategoryCollections("IsBitranslation");
#! @EndGroup

#! @BeginGroup XPartOfBitranslation
#! @GroupTitle XPartOfBitranslation
#! @Returns a left or right translation
#! @Arguments h
#! @Description
#! For a Bitranslation <A>h</A> consisting of a linked pair <M>(`l`, `r`)</M>,
#! of left and right translations, `LeftPartOfBitranslation(<A>b</A>)` returns
#! the left translation `l`, and `RightPartOfBitranslation(<A>b</A>)` returns
#! the right translation `r`.
DeclareGlobalFunction("LeftPartOfBitranslation");
DeclareGlobalFunction("RightPartOfBitranslation");
#! @EndGroup

#! @BeginGroup IsXTranslationsSemigroup
#! @GroupTitle IsXTranslationsSemigroup
#! @Description
#! `IsXTranslationsSemigroup` is a synonym for
#! `IsSemigroup and IsXTranslationCollection` (where `X` is one of `Semigroup`,
#! `Left` `Right`, or `Bi`).
DeclareSynonym("IsTranslationsSemigroup",
               IsSemigroup and IsSemigroupTranslationCollection);
DeclareSynonym("IsLeftTranslationsSemigroup",
               IsSemigroup and IsLeftTranslationCollection);
DeclareSynonym("IsRightTranslationsSemigroup",
               IsSemigroup and IsRightTranslationCollection);
DeclareSynonym("IsBitranslationsSemigroup",
               IsSemigroup and IsBitranslationCollection);
#! @EndGroup

#! @BeginGroup XTranslation
#! @GroupTitle XTranslation
#! @Returns a left or right translation
#! @Arguments T, x[, y]
#! @Description
#! For the semigroup <A>T</A> of left of right translations of a semigroup <A>
#! S</A> and <A>x</A> one of:
#! * a mapping on the underlying semigroup; note that in this case only the
#!   values of the mapping on the <Ref Attr="UnderlyingRepresentatives"/> of
#!   <A>T</A> are checked and used;
#! * a list of indices representing the images of the
#!   <Ref Attr="UnderlyingRepresentatives"/> of <A>T</A>, where the ordering
#!   is that of <Ref Oper="PositionCanonical"/> on <A>S</A>;
#! * (for `LeftTranslation`) a list of length `Length(Rows(S))` 
#!   containing elements of `UnderlyingSemigroup(S)`; in this case
#!   <A>S</A> must be a normalised Rees matrix semigroup and `y` must be
#!   a Transformation of `Rows(S)`;
#! * (for `RightTranslation`) a list of length `Length(Columns(S))` 
#!   containing elements of `UnderlyingSemigroup(S)`; in this case
#!   <A>S</A> must be a normalised Rees matrix semigroup and `y` must be
#!   a Transformation of `Columns(S)`;
#! `LeftTranslation` and `RightTranslation` return the corresponding
#! translations.
#! @BeginExampleSession
#! gap> S := RectangularBand(3, 4);;
#! gap> L := LeftTranslations(S);;
#! gap> s := AsList(S)[1];;
#! gap> f := function(x)
#! > return s * x;
#! > end;;
#! gap> map := MappingByFunction(S, S, f);;
#! gap> l := LeftTranslation(L, map);
#! <left translation on <regular transformation semigroup of size 12, 
#!  degree 8 with 4 generators>>
#! gap> s ^ l;
#! Transformation( [ 1, 2, 1, 1, 5, 5, 5, 5 ] )
#! @EndExampleSession
DeclareGlobalFunction("LeftTranslation");
DeclareGlobalFunction("RightTranslation");
#! @EndGroup

#! @Returns a bitranslation
#! @Arguments H, l, r
#! @Description
#! If <A>H</A> is a translational hull over a semigroup <M>S</M>, and <A>l</A>
#! and <A>r</A> are left and right translations respectively over <M>S</M>, then
#! this function returns the bitranslation <M>(<A>l</A>, <A>r</A>)</M>.
#! @BeginExampleSession
#! gap> G := Group((1, 2), (3, 4));;
#! gap> A := AsList(G);;
#! gap> mat := [[A[1], 0],
#! > [A[2], A[2]]];;
#! gap> S := ReesZeroMatrixSemigroup(G, mat);;
#! gap> L := LeftTranslations(S);;
#! gap> R := RightTranslations(S);;
#! gap> l := LeftTranslation(L, MappingByFunction(S, S, s -> S.1 * s));;
#! gap> r := RightTranslation(R, MappingByFunction(S, S, s -> s * S.1));;
#! gap> H := TranslationalHull(S);;
#! gap> x := Bitranslation(H, l, r);
#! <bitranslation on <regular semigroup of size 17, with 4 generators>>
#! @EndExampleSession
DeclareOperation("Bitranslation", 
  [IsBitranslationsSemigroup, IsLeftTranslation, IsRightTranslation]);

DeclareGlobalFunction("LeftTranslationNC");
DeclareGlobalFunction("RightTranslationNC");
DeclareGlobalFunction("BitranslationNC");

#! @BeginGroup UnderlyingSemigroup
#! @GroupTitle UnderlyingSemigroup
#! @Returns a semigroup
#! @Arguments S
#! @Description
#! Given a semigroup of translations or bitranslations, returns the
#! semigroup on which these translations act.
DeclareAttribute("UnderlyingSemigroup", IsTranslationsSemigroup);
DeclareAttribute("UnderlyingSemigroup", IsBitranslationsSemigroup);
#! @EndGroup

#! @BeginGroup XTranslationsSemigroupOfFamily
#! @GroupTitle XTranslationsSemigroupOfFamily
#! @Returns
#! the semigroup of left or right translations, or the translational hull
#! @Arguments fam
#! @Description
#! Given a family <A>fam</A> of left-, right- or bi-translations, returns
#! the translations semigroup or translational hull to which they belong.
#! @BeginExampleSession
#! gap> S := RectangularBand(3, 3);;
#! gap> L := LeftTranslations(S);;
#! gap> l := Representative(L);;
#! gap> LeftTranslationsSemigroupOfFamily(FamilyObj(l)) = L;
#! true
#! gap> H := TranslationalHull(S);;
#! gap> h := Representative(H);;
#! gap> TranslationalHullOfFamily(FamilyObj(h)) = H;
#! @EndExampleSession
DeclareAttribute("LeftTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("RightTranslationsSemigroupOfFamily", IsFamily);
DeclareAttribute("TranslationalHullOfFamily", IsFamily);
#! @EndGroup

#! @BeginGroup TypeXTranslationSemigroupElements
#! @GroupTitle TypeXTranslationSemigroupElements
#! @Returns a type
#! @Description
#! Given a (bi)translations semigroup, returns the type of the elements that
#! it contains.
DeclareAttribute("TypeLeftTranslationsSemigroupElements",
                 IsLeftTranslationsSemigroup);
DeclareAttribute("TypeRightTranslationsSemigroupElements",
                 IsRightTranslationsSemigroup);
DeclareAttribute("TypeBitranslations", IsBitranslationsSemigroup);
#! @EndGroup

#! @BeginGroup XTranslations
#! @GroupTitle XTranslations
#! @Returns the semigroup of left or right translations
#! @Arguments S
#! @Description
#! Given a finite semigroup <A>S</A> satisfying <Ref Filt="CanUseFroidurePin"/>,
#! returns the semigroup of all left or right translations of <A>S</A>. 
#! @BeginExampleSession
#! gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
#! > Transformation([3, 4, 1, 1, 4, 2])]);;
#! gap> L := LeftTranslations(S);
#! <the semigroup of left translations of <transformation semigroup of 
#!  degree 6 with 2 generators>>
#! gap> Size(L);
#! 361
#! @EndExampleSession
DeclareAttribute("LeftTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("RightTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
#! @EndGroup

#! @Returns the translational hull
#! @Arguments S
#! @Description
#! Given a finite semigroup <A>S</A> satisfying <Ref Filt="CanUseFroidurePin"/>,  
#! returns the translational hull of <A>S</A>.
#! @BeginExampleSession
#! gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
#! > Transformation([3, 4, 1, 1, 4, 2])]);;
#! gap> H := TranslationalHull(S);
#! <translational hull over <transformation semigroup of degree 6 with 2 
#!  generators>>
#! gap> Size(H);
#! 38
#! @EndExampleSession
DeclareAttribute("TranslationalHull",
                 IsSemigroup and CanUseFroidurePin and IsFinite);

#! @BeginGroup InnerXTranslations
#! @GroupTitle InnerXTranslations
#! @Returns the monoid of inner left or right translations
#! @Arguments S
#! @Description
#! For a finite semigroup <A>S</A> satisfying <Ref Filt="CanUseFroidurePin"/>,
#! <C>InnerLeftTranslations</C>(<A>S</A>) 
#! returns the inner left translations of S (i.e. the translations 
#! defined by left multiplication by a fixed element of <A>S</A>), and
#! <C>InnerRightTranslations</C>(<A>S</A>) returns the inner right translations
#! of <A>S</A> (i.e. the translations defined by right multiplication by
#! a fixed element of <A>S</A>).
#! @BeginExampleSession
#! gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
#! > Transformation([3, 4, 1, 1, 4, 2])]);;
#! gap> I := InnerLeftTranslations(S);
#! <left translations semigroup over <transformation semigroup of size 22, 
#!  degree 6 with 2 generators>>
#! gap> Size(I) <= Size(S);
#! true
#! @EndExampleSession
DeclareAttribute("InnerLeftTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
DeclareAttribute("InnerRightTranslations",
                 IsSemigroup and CanUseFroidurePin and IsFinite);
#! @EndGroup

#! @Returns the inner translational hull
#! @Arguments S
#! @Description
#! Given a finite semigroup <A>S</A> satisfying <Ref Filt="CanUseFroidurePin"/>,
#! returns the inner translational hull of <A>S</A>, i.e. the bitranslations 
#! whose left and right translation components are inner translations defined by
#! left and right multiplication by the same fixed element of <A>S</A>.
#! @BeginExampleSession
#! gap> S := Semigroup([Transformation([1, 4, 3, 3, 6, 5]),
#! > Transformation([3, 4, 1, 1, 4, 2])]);;
#! gap> I := InnerTranslationalHull(S);
#! <semigroup of bitranslations over <transformation semigroup of size 22, 
#!  degree 6 with 2 generators>>
#! gap> L := LeftTranslations(S);;
#! gap> R := RightTranslations(S);;
#! gap> H := TranslationalHull(S);;
#! gap> inners := [];;
#! gap> for s in S do
#! > l := LeftTranslation(L, MappingByFunction(S, S, x -> s * x));
#! > r := RightTranslation(R, MappingByFunction(S, S, x -> x * s));
#! > AddSet(inners, Bitranslation(H, l, r));
#! > od;
#! gap> Set(I) = inners;
#! true
#! @EndExampleSession
DeclareAttribute("InnerTranslationalHull",
                 IsSemigroup and CanUseFroidurePin and IsFinite);

InstallTrueMethod(IsFinite, IsLeftTranslationsSemigroup);
InstallTrueMethod(IsFinite, IsRightTranslationsSemigroup);
InstallTrueMethod(IsFinite, IsBitranslationsSemigroup);

InstallTrueMethod(CanUseGapFroidurePin, IsLeftTranslationsSemigroup);
InstallTrueMethod(CanUseGapFroidurePin, IsRightTranslationsSemigroup);
InstallTrueMethod(CanUseGapFroidurePin, IsBitranslationsSemigroup);

#! @Returns a set of representatives
#! @Arguments T
#! @Description
#! For efficiency, we typically store translations on a semigroup <M>S</M> as
#! their actions on a small subset of <M>S</S>. For left translations, this is a
#! set of representatives of the maximal &R;-classes of <M>S</M>; dually for
#! right translations we use representatives of the maximal &L;-classes. You can
#! use `UnderlyingRepresentatives` to access these representatives.
#! @BeginExampleSession
#! gap> G := Range(IsomorphismPermGroup(SmallGroup(12, 1)));;
#! gap> mat := [[G.1, G.2], [G.1, G.1], [G.2, G.3], [G.1 * G.2, G.1 * G.3]];;
#! gap> S := ReesMatrixSemigroup(G, mat);;
#! gap> L := LeftTranslations(S);;
#! gap> R := RightTranslations(S);;
#! gap> UnderlyingRepresentatives(L);
#! [ (1,(),1), (2,(),2) ]
#! gap> UnderlyingRepresentatives(R);
#! [ (1,(),1), (2,(),2), (1,(),3), (1,(),4) ]
#! @EndExampleSession
DeclareAttribute("UnderlyingRepresentatives", IsTranslationsSemigroup);

# Purposefully undocumented
DeclareAttribute("RepresentativeMultipliers", IsTranslationsSemigroup);

#! @Returns a set of elements
#! @Arguments t
#! @Description
#! Given a left or right translation <A>t</A> on a semigroup <M>S</M>, returns
#! the set of elements of <M>S</M> lying in the image of <A>t</A>.
#! @BeginExampleSession
#! gap> S := Semigroup([Transformation([1, 3, 3, 4]),
#! > Transformation([3, 4, 1, 2])]);;
#! gap> t := Set(LeftTranslations(S))[4];
#! <left translation on <transformation semigroup of size 8, degree 4 with 
#!  2 generators>>
#! gap> ImageSetOfTranslation(t);
#! [ Transformation( [ 1, 2, 3, 1 ] ), Transformation( [ 1, 3, 3, 1 ] ), 
#!   Transformation( [ 3, 1, 1, 3 ] ), Transformation( [ 3, 4, 1, 3 ] ) ]
#! @EndExampleSession
DeclareOperation("ImageSetOfTranslation", [IsSemigroupTranslation]);
