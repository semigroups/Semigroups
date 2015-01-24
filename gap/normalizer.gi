############################################################################
##
#W  normalizer.gi
#Y  Copyright (C) 2014-15                                James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

if not IsBound(POW_KER_PERM) then
  BindGlobal("POW_KER_PERM", function(pt, x)
    return FlatKernelOfTransformation(TransformationNC(pt) ^ x, Length(pt));
  end);
fi;

#

InstallMethod(Normalizer, "for a transformation semigroup and record",
[IsTransformationSemigroup, IsRecord],
function(S, opts)
  return Normalizer(SymmetricGroup(DegreeOfTransformationSemigroup(S)), S,
  opts);
end);

InstallMethod(Normalizer, "for a partial perm semigroup and record",
[IsPartialPermSemigroup, IsRecord],
function(S, opts)
  return Normalizer(SymmetricGroup(DegreeOfPartialPermSemigroup(S)), S, opts);
end);

InstallMethod(Normalizer, "for a bipartition semigroup and record",
[IsBipartitionSemigroup, IsRecord],
function(S, opts)
  return Normalizer(SymmetricGroup(DegreeOfBipartitionSemigroup(S)), S, opts);
end);

#

InstallMethod(Normalizer, "for a transformation semigroup",
[IsTransformationSemigroup],
function(S)
  return DeterministicSemigroupNormalizer(
   SymmetricGroup(DegreeOfTransformationSemigroup(S)), S, rec());
end);

InstallMethod(Normalizer, "for a partial perm semigroup",
[IsPartialPermSemigroup],
function(S)
  return DeterministicSemigroupNormalizer(
   SymmetricGroup(DegreeOfPartialPermSemigroup(S)), S, rec());
end);

InstallMethod(Normalizer, "for a bipartition semigroup",
[IsBipartitionSemigroup],
function(S)
  return DeterministicSemigroupNormalizer(
   SymmetricGroup(DegreeOfBipartitionSemigroup(S)), S, rec());
end);

#

InstallMethod(NormalizerOp,
"for a permutation group and a semigroup",
[IsPermGroup, IsSemigroup],
function(G, S)
  return DeterministicSemigroupNormalizer(G, S, rec()); #i.e. deterministic
end);

#

if IsBound(GAPInfo.PackagesLoaded.genss) then

  InstallMethod(Normalizer,
  "for a permutation group, a semigroup, a record",
  [IsPermGroup, IsSemigroup, IsRecord],
  function(G, S, opts)
    if IsBound(opts.random) and opts.random then
      return NonDeterministicSemigroupNormalizer(G, S, opts);
    else
      return DeterministicSemigroupNormalizer(G, S, opts);
    fi;
  end);

else

  InstallMethod(Normalizer,
  "for a permutation group, a semigroup, a record",
  [IsPermGroup, IsSemigroup, IsRecord],
  DeterministicSemigroupNormalizer);

fi;

# process the options record...

BindGlobal("NormalizerOptsRec@",
function(S, opts)

  # don't check the component `random' since this is only called inside
  # (Non)DeterministicSemigroupNormalizer...

  if not IsBound(opts.lambdastab) then
    opts.lambdastab := true;
  elif not IsBool(opts.lambdastab) then
    Error("Semigroups: NormalizerOptsRec@: usage,\n",
          "the component `lambdastab' must be a boolean,");
    return;
  fi;

  if (IsPartialPermSemigroup(S) and IsInverseSemigroup(S)) then
    opts.rhostab := false;
  elif not IsBound(opts.rhostab) then
    opts.rhostab := true;
  elif not IsBool(opts.rhostab) then
    Error("Semigroups: NormalizerOptsRec@: usage,\n",
          "the component `rhostab' must be a boolean,");
    return;
  fi;

  return opts;
end);

# process the lambda-orb

BindGlobal("LambdaOrbForNormalizer@",
function(G, S, func)
  local o, i, nr;

  o := LambdaOrb(S);
  Enumerate(o, infinity);
  o := ShallowCopy(o);
  Remove(o, 1);
  Sort(o, func);

  if IsTransformationSemigroup(S) or IsPartialPermSemigroup(S) then

    if IsEmpty(o[1]) then
      Remove(o, 1);
    fi;
    i := (MinActionRank(S) + 1) mod 2 + 1;
    nr := i - 1;
    while nr < Length(o) and Length(o[nr + 1]) = 1 do
      nr := nr + 1;
    od;

    if o{[i .. nr]} = NrMovedPoints(G) then
      for i in [i .. nr] do
        Remove(o, 1);
      od;
    fi;
  fi;
  return o;
end);

#

InstallGlobalFunction(DeterministicSemigroupNormalizer,
function(G, S, opts)
  local o, i, nr, act, deg, U, gens, nrgens, P;

  if not IsPermGroup(G) then
    Error("Semigroups: DeterministicSemigroupNormalizer: usage,\n",
          "the first arg must be a permutation group,");
    return;
  fi;

  if not (IsTransformationSemigroup(S) or IsPartialPermSemigroup(S) or
    IsBipartitionSemigroup(S)) then
    Error("Semigroups: DeterministicSemigroupNormalizer: usage,\n",
          "the second arg must be a semigroup of transformations,\n",
          "partial perms or bipartitions,");
    return;
  fi;

  if not IsRecord(opts) then
    Error("Semigroups: DeterministicSemigroupNormalizer: usage,\n",
          "the third argument must be a record,");
    return;
  fi;

  if IsTrivial(G) then
    return G;
  fi;

  opts := NormalizerOptsRec@(S, opts);

  # the example of the JonesMonoid(8), and the inverse semigroup example
  # immediately after it in normalizer.tst show that calculating the stabilizer
  # of lambda/rho values isn't always worth it. But then the test file is 2
  # second quicker with this than without...

  if opts.lambdastab then
    o := LambdaOrbForNormalizer@(G, S, LT);

    if IsTransformationSemigroup(S) or IsPartialPermSemigroup(S) then
      act := OnSetsSets;
    else
      deg := DegreeOfBipartitionSemigroup(S);
      act := function(pt, x)
             x := AsBipartition(x, deg);
             return Set(pt, y -> RightBlocks(ProjectionFromBlocks(y) * x));
           end;
    fi;

    Info(InfoSemigroups, 2, "finding the stabilizer of the lambda values...");
    U := Stabilizer(G, o, act);
    Info(InfoSemigroups, 2, Size(U), " found");
  else
    U := G;
  fi;

  if Size(U) > 1 and opts.rhostab then
    o := RhoOrb(S);
    Enumerate(o, infinity);
    o := ShallowCopy(o);
    Remove(o, 1);
    Sort(o);

    if IsTransformationSemigroup(S) then
      act := function(pt, x)
        return Set(pt, y -> POW_KER_PERM(y, x));
      end;
    elif IsPartialPermSemigroup(S) then
      act := RhoAct(S);
    else
      deg := DegreeOfBipartitionSemigroup(S);
      act := function(pt, x)
             x := AsBipartition(x ^ - 1, deg);
             return Set(pt, y -> LeftBlocks(x * ProjectionFromBlocks(y)));
           end;
    fi;

    Info(InfoSemigroups, 2, "finding the stabilizer of the rho values...");
    U := Stabilizer(U, o, act);
    Info(InfoSemigroups, 2, Size(U), " found");
  fi;

  gens := Generators(S);
  nrgens := Length(gens);

  if Size(U) = 1 or nrgens = 0 then
    return U;
  fi;

  P := function(x)
    local i, pt;
    i := 0;
    repeat
      i := i + 1;
      pt := gens[i];
    until i = nrgens or (not pt ^ x in S);
    return i = nrgens and pt ^ x in S;
  end;

  return SubgroupProperty(U, P);
end);

#

if IsBound(GAPInfo.PackagesLoaded.genss) then

  InstallGlobalFunction(NonDeterministicSemigroupNormalizer,
  function(G, S, opts)
    local o, act, deg, U, gens, nrgens, P, pruner, out;

    Info(InfoWarning, 1,
      "This function uses random methods and so there is some chance that");
    Info(InfoWarning, 1,
      "it will return an incorrect result. Call the function with the option");
    Info(InfoWarning, 1,
      "`random' set to <false> for a deterministic (but slower) answer.");

    if not IsPermGroup(G) then
      Error("Semigroups: NonDeterministicSemigroupNormalizer: usage,\n",
            "the first arg must be a permutation group,");
      return;
    fi;

    if not (IsTransformationSemigroup(S) or IsPartialPermSemigroup(S) or
      IsBipartitionSemigroup(S)) then
      Error("Semigroups: NonDeterministicSemigroupNormalizer: usage,\n",
            "the second arg must be a semigroup of transformations,\n",
            "partial perms or bipartitions,");
      return;
    fi;

    if not IsRecord(opts) then
      Error("Semigroups: NonDeterministicSemigroupNormalizer: usage,\n",
            "the third arg must be a record,");
      return;
    fi;

    if IsTrivial(G) then
      return G;
    fi;

    opts := NormalizerOptsRec@(S, opts);

    if opts.lambdastab then

      if IsTransformationSemigroup(S) or IsPartialPermSemigroup(S) then
        act := OnSets;
        o := LambdaOrbForNormalizer@(G, S,
          function(x,y)
            return Length(x) < Length(y);
          end);
      else
        deg := DegreeOfBipartitionSemigroup(S);
        act := function(pt, x)
                 return RightBlocks(ProjectionFromBlocks(pt)
                 * AsBipartition(x, deg));
               end;
        o := LambdaOrbForNormalizer@(G, S,
          function(x,y)
            return NrBlocks(x) < NrBlocks(y);
          end);
      fi;
      Info(InfoSemigroups, 2, "finding the stabilizer of the images...");
      U := SetwiseStabilizer(G, act, o).setstab;
      Info(InfoSemigroups, 2, Size(U), " found");
    else
      U := G;
    fi;

    if Size(U) > 1 and opts.rhostab then
      o := RhoOrb(S);
      Enumerate(o, infinity);
      o := ShallowCopy(o);
      Remove(o, 1);
      Sort(o, function(x, y)
                return Maximum(x) < Maximum(y);
              end);

      if IsTransformationSemigroup(S) then
        act := POW_KER_PERM;
      elif IsPartialPermSemigroup(S) then
        act := RhoAct(S);
      else
        deg := DegreeOfBipartitionSemigroup(S);
        act := function(pt, x)
          return LeftBlocks(AsBipartition(x ^ - 1, deg)
          * ProjectionFromBlocks(pt));
        end;
      fi;
      Info(InfoSemigroups, 2, "finding the stabilizer of the kernels...");
      U := SetwiseStabilizer(U, act, o).setstab;
      Info(InfoSemigroups, 2, Size(U), " found");
    fi;

    if Size(U) = 1 then
      return U;
    fi;

    gens := Generators(S);
    nrgens := Length(gens);

    # recalculate the stabilizer chain using the generators of <S> as the base
    # points
    U := StabilizerChain(U, rec( Cand := rec( points := gens,
     ops := ListWithIdenticalEntries(nrgens, OnPoints), used := 0),
     StrictlyUseCandidates := true));

    P := function(x)
    local i, pt;
      i := 0;
      repeat
        i := i + 1;
        pt := gens[i];
      until i = nrgens or (not pt ^ x in S);
      return i = nrgens and pt ^ x in S;
    end;

    pruner := function(stabchain, index, tg, t, word)
      return stabchain!.orb[1] ^ tg in S;
    end;

    U := BacktrackSearchStabilizerChainSubgroup(U, P, pruner);
    out := Group(U!.orb!.gens);
    SetStabilizerChain(out, U);
    return out;
  end);
fi;

#EOF
