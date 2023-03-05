#############################################################################
##
#W  extreme/cong.tst
#Y  Copyright (C) 2014-15                                   Wilf A. Wilson
##                                                          Michael Young
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

#@local P, R, S, c, classes, classx, classy, classz, cong, gens, l, q, s, t, u
#@local v, x, y, z
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
<2-sided semigroup congruence over <transformation semigroup of degree 6 with 
 6 generators> with 2 generating pairs>
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
Error, the 1st argument (a list) does not have length 2
gap> [x, Transformation([1])] in cong;
Error, the items in the 1st argument (a list) do not all belong to the range o\
f the 2nd argument (a 2-sided semigroup congruence)
gap> classes := EquivalenceClasses(cong);;
gap> Size(classes) = NrEquivalenceClasses(cong);
true
gap> classx := EquivalenceClassOfElement(cong, x);;
gap> classy := EquivalenceClassOfElement(cong, y);;
gap> classz := EquivalenceClassOfElement(cong, z);
<2-sided congruence class of Transformation( [ 2, 4, 6, 1, 6, 5 ] )>
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
gap> v := SemigroupCongruence(t, [gens[1], gens[1]]);;
gap> v = SemigroupCongruence(t, []);
true
gap> NrEquivalenceClasses(v);
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
<2-sided semigroup congruence over <commutative non-regular transformation 
 monoid of size 6, degree 10 with 1 generator> with 5 generating pairs>
gap> u = v;
true
gap> NrEquivalenceClasses(u);
1

# PairsCongTest4: \* for two semigroups congruence classes
gap> gens := [Transformation([2, 6, 7, 2, 6, 9, 9, 1, 1, 5])];;
gap> s := Semigroup(gens);;
gap> Size(s);
5
gap> IsRegularSemigroup(s);
false
gap> gens := List(s, x -> [gens[1], x]);;
gap> u := SemigroupCongruence(s, gens);  # universal congruence
<2-sided semigroup congruence over <commutative non-regular transformation 
 semigroup of size 5, degree 10 with 1 generator> with 4 generating pairs>
gap> u = UniversalSemigroupCongruence(s);
true
gap> v := SemigroupCongruence(s, [gens[1], gens[1]]);  # trivial congruence
<2-sided semigroup congruence over <commutative non-regular transformation 
 semigroup of size 5, degree 10 with 1 generator> with 0 generating pairs>
gap> classes := Set(EquivalenceClasses(v));
[ <2-sided congruence class of Transformation( [ 2, 6, 7, 2, 6, 9, 9, 1, 1,
      5 ] )>, <2-sided congruence class of Transformation( [ 6, 9, 9, 6, 9, 1,
     1, 2, 2, 6 ] )>, <2-sided congruence class of Transformation( [ 9, 1, 1,
      9, 1, 2, 2, 6, 6, 9 ] )>, 
  <2-sided congruence class of Transformation( [ 1, 2, 2, 1, 2, 6, 6, 9, 9,
      1 ] )>, <2-sided congruence class of Transformation( [ 2, 6, 6, 2, 6, 9,
     9, 1, 1, 2 ] )> ]
gap> ForAny(EquivalenceClasses(u), x -> x in classes);
false
gap> classes[1] * EquivalenceClasses(u)[1];
Error, the arguments (cong. classes) are not classes of the same congruence
gap> EquivalenceClasses(u)[1] * classes[1];
Error, the arguments (cong. classes) are not classes of the same congruence
gap> classes[3] * classes[4];
<2-sided congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6,
  9 ] )>
gap> classes[4] * classes[3];
<2-sided congruence class of Transformation( [ 9, 1, 1, 9, 1, 2, 2, 6, 6,
  9 ] )>
gap> Representative(classes[5] * classes[2]) =
> Representative(classes[5]) * Representative(classes[2]);
true

# LatticeOfCongruences
gap> S := Semigroup([
> Transformation([1, 3, 4, 1]), Transformation([3, 1, 1, 3])]);;
gap> l := LatticeOfCongruences(S);
<lattice of 52 two-sided congruences over <transformation semigroup 
 of size 12, degree 4 with 2 generators>>
gap> IsIsomorphicDigraph(l, 
> DigraphReflexiveTransitiveClosure(DigraphFromDiSparse6String(
> Concatenation(".sa?wwQFR@t`\\QqNIdyLUyGHpZsuxJbfYqND@QVxgxNANVl`LVO",
> "TRbauTneHUDJAgV\\GgI@GBoXoa_BEbh{]k@kBqkAKX_VQtAZCHQ",
> "hdQiVLSRAQKZoshppN[dqQtLVJMrIc{nbQkKQ|d"))));
true
gap> S := Semigroup([
> Transformation([1, 4, 3, 1, 4, 2]), Transformation([1, 6, 6, 3, 6, 6])]);;
gap> IsRegularSemigroup(S);
false
gap> l := LatticeOfCongruences(S);
<lattice of 5 two-sided congruences over <non-regular transformation 
 semigroup of size 48, degree 6 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphFromDigraph6String("&D}{ho_"));
true
gap> S := Semigroup([
> Transformation([4, 3, 1, 1, 6, 4]), Transformation([4, 3, 6, 4, 2, 3])]);;
gap> l := LatticeOfCongruences(S);
<lattice of 328 two-sided congruences over <transformation semigroup 
 of size 21, degree 6 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphReflexiveTransitiveClosure(
> DigraphFromDiSparse6String(".~?DG__?gB_wBGO?uDOS`[DWR`oEg^@iGO]a_C?fak\
> BwEaw?_jau@?hA}IGNbAMOMBaMoP`EDG~@kFOa`oFxA?YPPC`ENPD_UJhJCiAGJaWRXOAS\
> RxQCyL@DdAT_X_YJ@VdkUhXboP`[eGJov_}Wwze[NPecEOhBc[Spf_iZHe`[XpmcaXGGAw\
> [XHFILPJfWMwjBI]`xfw]H}fm]wJgI]YG@IHh`gebP{fsaGlGM^iCgSbHw`OcyFGuJPMGe\
> aqNHYeaWaGeiWaGfI_H}gqGi[Mo{`iFqg`Whw]IeeWZHiGIe`oiG`ImgYcJAhQpjWL_}hi\
> mOZHqfIyaAfQyb{fQxjknZAHUeQvjISXUaoOjHCGTzDk{ribacdyd`McySLQcqj`SdBUdo\
> jgUDsuHDJCtgVC[trYdKlBQLiE@SLMeYYHqeq\\L}NqZIuOAhJAeqvdUx`CI}UX[M]VQfM\
> amy~MiUBFKeUhZMuYX~HARq?LACicjKrraNMqrgnIUQkKMVAwbWVRwnkaaKnsqBL`Q\\Xd\
> FY^qSNy~{?eW{C@h{viceShynJL@jckwxWsMXAJhNT@wTNyIpbf\\BhyGQ[SNhDCIiO]YR\
> FmpDJKMyZbQOQYpnLkvCVly]Aejgvy|M@E{]IaiS]qDFY^PmgC[QIis^f{tccg?_[gHzHy\
> ELBGCgiDGShqJGslhyf{qIovilJcvy}M?wSriwnCsq}|SpiG|jgNayRxReuBgRUibXMdMz\
> fN\\Ki_R@LIePz@SXQI^cYR~JT?h@@d@oTKCqklNTCl\\?SLQVBCRR\\NLG_A@o@_c@wB_\
> w@wA`{Fgb?QBWf?EI_IbC?Gi_EM?L`?HwcBaN?U`]@g_aE?gNbuLHEBWNgA_i?wtce@?na\
> sRHN@MEGe_OIXSCq@gDdY@XZDaVOE_wMGbCqM`]aWRWFBMAp`__RWQc}SGGD}WwdE]A@S_\
> oVgH?yCwWe?ZGla}APab[[wNCED@B_gQXCciQpvc}Axzg?AwRauBIDGMCweggJQHbabPLD\
> }HPN_wWgsbULqNcS^wNC]RghbeEo\\AICYX`Ef_Qi?XXNgYRqEhAZgS`Q]Ae`OcwT`UNHX\
> IqDg|cOjwVdALHnbwTWXD]UWu`iEg[`yFw^JqOHPG]HQqaKShEJUT@^geI`GamabDbGpgy\
> Gm^iCgqkxieEHorhEJQQfeK`yfeIpydiJ@ZciJ`JbUJovbmfr]bkgG{Dojg|CSkXEDomHA\
> FUOqncCPxwKmcxAHQkBMMUOrNMIQ@vfcrHyMqSP`KaSrQcsTBRiGgxeIS{HRE[lH~l?zxT\
> DuUhVDmWQFGuVaJG}oBnNiWBINmX`zIMWp{Ni^QGNyWq?LAX@gGuX`iGyX@jNEXQ]eWgZq\
> NyXqqec~B~egoSGe_YrCec|c@ew_QEfC_aIeo_rzgSbCNes[qNfOccPiHBZGOy[B|O}rSO\
> PU[RRPI[`sLu\\aeMU\\QSMYiBiPe\\ahMm]q`fwhH~NK~h|Pu^A?pz?C^iTBYCLI_aDLM\
> gQbg`HzONiaaKLMba`KEbRCNEhBMk{{sBnHHzJPzCsciWhygM\\KCqi[iRgigispnW~Chk\
> pHZGKtIjSMuqRTMytblR]prVMyxBhPi|SFP|KZ}OXFSfo@GCgR}~s@QDI[BOXAcjoH@CJQ\
> r@snR~ASKRXO[MP@DseQjDCUR`Pn")));
true
gap> S := Semigroup([
> Transformation([1, 5, 4, 5, 2]), Transformation([4, 5, 1, 3, 5])]);;
gap> l := LatticeOfCongruences(S);
<lattice of 207 two-sided congruences over <transformation semigroup 
 of size 74, degree 5 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphReflexiveTransitiveClosure(
> DigraphFromDiSparse6String(".~?BN`_G`wLaWObwABSeCKZcKk?[n?WjaCJaCtDSXE\
> kEeCDfWCCSaFSHg?FA[NaLAg[kh@ChWWhhAcXKi?liOFE@N_wqb`Ic[M`tIh\\ZBcxjwbk\
> HZjtNiDPg@QK\\?I\\YjsRdtCc`jk[pKcJKpmao\\ccLjSHJd\\L\\]k@kbh`kPvl@pMdh\
> MPval|nsTdkWOMEOKPoiEoEJOYCoQEni?n}RAyPdqPeLReTtmtwnKdNS]N[dNcKRE`?Gki\
> DnsgaSEf@Op_pRscsRS_FadstKKlBHlMJyli`WJ\\TJ{~H@kipVPlEHYAgpcjIsUkVGyuh\
> YQiIQqEQjIwV\\dVIzkyUw@cST^V{vKygjinWL}PmbV{cWJDlhuWZExIVbBHpRHdkqFEXT\
> JFx{I?KBa?FaODa_B`sZ?S\\B[Y_CcCS`b{YbS]b{n?KqDkie[u?CI_sHcCACsZ_OieW|e\
> _}`[BD\\D?_ac[cGkCfxGhgDbdOGtK_tO_lLIcFH{LasVjW\\`D^Cd[jTYkgme{pKd[k@g\
> `WSfSHbg{d[kG{ID{HFs~L`p`X@Ls]`dvCltdHsmdtn@zi{L`{OItEaLIbdKc[QHlMOcoH\
> |POkQIdTOyJa[SGKUBk[HScc[UMTlP{VGyPgpJQSWOIUdS_rC]C{]DCrRCsRQ\\bGtcCaF\
> Q_cXKc`jSMXSC`ckmQIbiIQSSdSkeDSfDChRd\\L[xK@kjXlfHgMLhMQmUCzFiig@bM\\A\
> IlFJ|BULCHHqhPThqOj@ZP|^QLWQYpqanVTXQitV\\\\jybTi~jtdSyzWD`KTiMYMmqcmy\
> eTLxNQkoYCQEEQUBPawpAxXEDOqapQgXJKrY[T^")));
true
gap> S := Semigroup([
> Transformation([1, 3, 5, 5, 3]), Transformation([4, 5, 5, 5, 2])]);;
gap> l := LatticeOfCongruences(S);
<lattice of 225 two-sided congruences over <transformation semigroup 
 of size 18, degree 5 with 2 generators>>
gap> IsIsomorphicDigraph(l, DigraphReflexiveTransitiveClosure(
> DigraphFromDiSparse6String(".~?B``wKaoPb_Xbo[_wN__Fc_`dOTb[GCCr?St?krb\
> CxE{xfgze\\??kWatC?sXc|MGlLaLQBWta_[bpRdCjISRCtXHd[JTOaD_Ecik_jkpc`WhH\
> DjLDmKLo@SZipckpqmxta\\yBL{JkeN[P`sSNSdNcoKECB]ELdlOkSGkahqGiPpiYGpqLa\
> gpePQOc@qWao[jFQCjY@hxZdEYIqJr`iQ}Xf{mex@egxr{lSSvsopFqdeStGA_fagsIgfa\
> ]iaKdOlP|TRDC_Ynh[wu_{MMrag}mCHU|~`ixrCfV\\BHu~VsmIqOQmkgR@dyAvmDouFWl\
> hpYhUdqRVKX^MLLuRYtmZIyDmYMIi@}Y]?PEdXMNSquYuJQAff[|ZMYRz?ZEmV\\EVb[ly\
> jUeaYrZsYlYz^{KH?[E_{F_[Ea?KaONagPawC_kSaSXbgEbk?cW?cCQcwL`sk?Cn?Kndkm\
> agpeCqdOlbWtecjds|?Ks_SAfx?_[P_\\B`KC?{C`XFHCKG{OHSRcKa__caWedHGadS?sI\
> iwKASVG{YI{`_{FCcdI{JDKOI{RJ{UaxGjH^bcIBk]KkGCC`cSGc`fjh^cp^`HAahB`HCg\
> hni[IIdHJ@ajPg`XZ`X[L\\IJCMBk]NLY``Z``[`hA`hC`pS`|JNCgNLyO\\za@{a@|aKe\
> N\\OPKTPCUhkVH{fNsgODPKSYCkeJLTKleQc^C[_cx~lCidKbDchL[rL{lf`pRsmIszdw|\
> eOyOeaeAbeI^SkrPYffqdeqYR{wFtAGdELtEMDLJTFHxZJdHIH]h`_K\\LLDMLLGHxji@_\
> L`lminU\\HMpvUAtnYnhP{NiooixhYEOyyh`OQ]XU\\PRY\\UlTM]LT|WPqomaPULXJiQm\
> iXjpuR\\]My\\liDPMIUrGkIKpirkQMUlbLbCkXlQ\\fLQVlpoTevWJJmawWRKvJ@nAyWT\
> }NykY]zY]AVbSpyWYjUvrJqI~XelTrOuy}WmpVA~WrFuQuVjLXvPZJ[wZQZR\\")));
true
gap> S := OrderEndomorphisms(2);;
gap> l := LatticeOfCongruences(S);
<lattice of 3 two-sided congruences over <regular transformation monoid 
 of size 3, degree 2 with 2 generators>>
gap> OutNeighbours(DigraphReflexiveTransitiveReduction(l));
[ [ 3 ], [  ], [ 2 ] ]
gap> S := OrderEndomorphisms(3);;
gap> l := LatticeOfCongruences(S);
<lattice of 4 two-sided congruences over <regular transformation monoid 
 of size 10, degree 3 with 3 generators>>
gap> IsIsomorphicDigraph(DigraphFromDigraph6String("&C|a["), l);
true
gap> S := OrderEndomorphisms(4);;
gap> l := LatticeOfCongruences(S);
<lattice of 5 two-sided congruences over <regular transformation monoid 
 of size 35, degree 4 with 4 generators>>
gap> IsIsomorphicDigraph(DigraphFromDigraph6String("&D}wof_"), l);
true
gap> S := PartitionMonoid(2);;
gap> l := LatticeOfCongruences(S);
<lattice of 13 two-sided congruences over <regular bipartition *-monoid 
 of size 15, degree 2 with 3 generators>>
gap> IsIsomorphicDigraph(l, 
> DigraphFromDigraph6String("&L~~gpU{yksMEB@?_?XozWKcAI@B?__"));
true

#
gap> SEMIGROUPS.StopTest();
gap> STOP_TEST("Semigroups package: extreme/cong.tst");
