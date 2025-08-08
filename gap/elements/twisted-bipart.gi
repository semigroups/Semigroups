############################################################################
##
##  elements/twisted-bipart.gi
##  Copyright (C) 2025                                   James D. Mitchell
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
## In collaboration with
##
##############################################
##                                          ##
##               Code created               ##
##                    by                    ##
##          *--------------------*          ##
##          | Matthias Fresacher |          ##
##          *--------------------*          ##
##                                          ##
##############################################

#    *------------------------------*
#    |``````````````````````````````|
#    |`````____````____`````````````|
#    |````|MFMF\  /MFMF|````````````|
#    |````|MF|MF\/MF|MF|````````````|
#    |````|MF|\MFMF/|MF|_______`````|
#    |````|MF|``````|MFMFMFMFMF|````|
#    |````|MF|``````|MF|````````````|
#    |````|MF|``````|MF|___`````````|
#    |``````````````|MFMFMF|````````|
#    |``````````````|MF|````````````|
#    |``````````````|MF|````````````|
#    |``````````````|MF|````````````|
#    |``````````````````````````````|
#    *------------------------------*

BindGlobal("TYPES_TWISTED_BIPART", []);
BindGlobal("TYPE_TWISTED_BIPART",
function(n, d)
  local fam, type;

  if IsBound(TYPES_TWISTED_BIPART[n + 1]) then
    if IsBound(TYPES_TWISTED_BIPART[n + 1][d + 1]) then
      return TYPES_TWISTED_BIPART[n + 1][d + 1];
    fi;
  else
    TYPES_TWISTED_BIPART[n + 1] := [];
  fi;

  fam := NewFamily(Concatenation("TwistedBipartitionFamily",
                                 String(n),
                                 "_",
                                 String(d)),
                   IsTwistedBipartition,
                   CanEasilySortElements,
                   CanEasilySortElements);
  fam!.DegreeOfBipartition := n;
  fam!.MaxFloatingBlocks := d;

  type := NewType(fam, IsTwistedBipartition and IsPositionalObjectRep);
  TYPES_TWISTED_BIPART[n + 1][d + 1] := type;
  return type;
end);

InstallMethod(ZeroTwistedBipartition,
"for degree and max. floating blocks", [IsInt, IsInt],
function(n, d)
  local result;
  if n >= 2 ^ 29 then
    ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
  fi;
  if d < 0 then
    ErrorNoReturn("the maximum number of floating blocks must be non-negative");
  fi;
  result := [];
  Objectify(TYPE_TWISTED_BIPART(n, d), result);
  return result;
end);

InstallMethod(TwistedBipartition,
"for number of floating blocks, bipartition and max. floating blocks (d)",
[IsInt, IsBipartition, IsInt],
function(i, bipart, d)
  local n, result;
  if d < 0 then
    ErrorNoReturn("the maximum number of floating blocks must be non-negative");
  fi;
  if i < 0 then
    ErrorNoReturn("the number of floating blocks must be non-negative");
  elif i > d then
    ErrorNoReturn("the number of floating blocks cannot exceed the maximum number of floating blocks");
  fi;
  n := DegreeOfBipartition(bipart);
  result := [i, bipart];
  Objectify(TYPE_TWISTED_BIPART(n, d), result);
  return result;
end);

InstallMethod(MaxFloatingBlocks, "for a twisted bipartition",
[IsTwistedBipartition],
x -> FamilyObj(x)!.MaxFloatingBlocks);

InstallMethod(DegreeOfTwistedBipartition, "for a twisted bipartition",
[IsTwistedBipartition],
x -> FamilyObj(x)!.DegreeOfBipartition);

InstallMethod(IsZero, "for a twisted bipartition",
[IsTwistedBipartition], x -> not IsBound(x![1]));

InstallMethod(IsOne, "for a twisted bipartition",
[IsTwistedBipartition], x -> One(x)=x);

InstallMethod(NrFloatingBlocks, "for a twisted bipartition",
[IsTwistedBipartition],
function(x) 
    if IsZero(x) then
      ErrorNoReturn("the zero diagram does not have floating blocks");
    fi;
    return x![1];
end);

InstallMethod(UnderlyingBipartition, "for a twisted bipartition",
[IsTwistedBipartition],
function(x) 
    if IsZero(x) then
      ErrorNoReturn("the zero diagram does not have an underlying bipartition");
    fi;
    return x![2];
end);

InstallMethod(Zero, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
  local n, d;
  if IsZero(x) then
    return x;
  fi;

  n := DegreeOfBipartition(UnderlyingBipartition(x));
  d := MaxFloatingBlocks(x);
  return ZeroTwistedBipartition(n, d);
end);

InstallMethod(One, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
  local n, d;
  n := DegreeOfBipartition(UnderlyingBipartition(x));
  d := MaxFloatingBlocks(x);
  return TwistedBipartition(0, IdentityBipartition(n), d);
end);

InstallMethod(IdentityTwistedBipartition,
"for degree n and max. floating blocks (d)",
[IsPosInt, IsInt],
function(n, d)
  if n >= 2 ^ 29 then
    ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
  fi;
  if d < 0 then
    ErrorNoReturn("the maximum number of floating blocks must be non-negative");
  fi;
  return TwistedBipartition(0, IdentityBipartition(n), d);
end);

InstallMethod(ViewString, "for a twisted bipartition",
[IsTwistedBipartition],
function(x)
   if IsZero(x) then
       return StringFormatted("ZeroTwistedBipartition({}, {})",
       DegreeOfTwistedBipartition(x), MaxFloatingBlocks(x));
   fi;
   return StringFormatted("TwistedBipartition({}, {}, {})",
   NrFloatingBlocks(x), PrintString(UnderlyingBipartition(x)),
   MaxFloatingBlocks(x));
end);

InstallMethod(RandomNonZeroTwistedBipartition,
"for a degree and max. floating blocks", [IsInt, IsInt],
function(n, d)
    if n >= 2 ^ 29 then
      ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
    fi;
    if d < 0 then
      ErrorNoReturn("the maximum number of floating blocks must be non-negative");
    fi;
    return TwistedBipartition(Random([0 .. d]), RandomBipartition(n), d);
end);

InstallMethod(RandomNonZeroTwistedBipartition,
"for a random source, degree and max. floating blocks", [IsRandomSource, IsInt, IsInt],
function(rs, n, d)
    if n >= 2 ^ 29 then
      ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
    fi;
    if d < 0 then
      ErrorNoReturn("the maximum number of floating blocks must be non-negative");
    fi;
    return TwistedBipartition(Random(rs, [0 .. d]), RandomBipartition(rs, n), d);
end);

InstallMethod(RandomTwistedBipartition,
"for a degree and max. floating blocks", [IsInt, IsInt],
function(n, d)
    # Gives 1% chance of selecting zero
    if n >= 2 ^ 29 then
      ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
    fi;
    if d < 0 then
      ErrorNoReturn("the maximum number of floating blocks must be non-negative");
    fi;
    if Random(GlobalMersenneTwister, 1, 100) = 16 then
        return Zero(TwistedBipartition(Random([0 .. d]), RandomBipartition(n), d));
    else
        return TwistedBipartition(Random([0 .. d]), RandomBipartition(n), d);
    fi;
end);

InstallMethod(RandomTwistedBipartition,
"for a random source, degree and max. floating blocks", [IsRandomSource, IsInt, IsInt],
function(rs, n, d)
    # Gives 1% chance of selecting zero
    if n >= 2 ^ 29 then
      ErrorNoReturn("the degree (a positive integer) must not exceed 2 ^ 29 - 1");
    fi;
    if d < 0 then
      ErrorNoReturn("the maximum number of floating blocks must be non-negative");
    fi;
    if Random(rs, 1, 100) = 16 then
        return Zero(TwistedBipartition(Random([0 .. d]), RandomBipartition(rs, n), d));
    else
        return TwistedBipartition(Random([0 .. d]), RandomBipartition(rs, n), d);
    fi;
end);

InstallMethod(\*, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
  local xx, yy, floaters;

  if IsZero(x) then
    return x;
  elif IsZero(y) then
    return y;
  fi;
  xx := UnderlyingBipartition(x);
  yy := UnderlyingBipartition(y);

  floaters := NrFloatingBlocks(xx, yy)
              + NrFloatingBlocks(x) + NrFloatingBlocks(y);

  if floaters > MaxFloatingBlocks(x) then
    return ZeroTwistedBipartition(DegreeOfTwistedBipartition(x),
                                  MaxFloatingBlocks(x));
  fi;

  return TwistedBipartition(floaters, xx * yy, MaxFloatingBlocks(x));
end);

InstallMethod(\=, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
  if IsZero(x) then
    return IsZero(y);
  elif IsZero(y) then
    return false;
  fi;
  return NrFloatingBlocks(x) = NrFloatingBlocks(y)
    and UnderlyingBipartition(x) = UnderlyingBipartition(y);
end);

InstallMethod(\<, "for twisted bipartition", IsIdenticalObj,
[IsTwistedBipartition, IsTwistedBipartition],
function(x, y)
  if IsZero(x) then
    return not IsZero(y);
  elif IsZero(y) then
    return false;
  fi;
  return NrFloatingBlocks(x) < NrFloatingBlocks(y) or
    (NrFloatingBlocks(x) = NrFloatingBlocks(y)
    and UnderlyingBipartition(x) < UnderlyingBipartition(y));
end);

SEMIGROUPS.TwistedBipartitionHashFunc := function(x, data)
  if IsZero(x) then
    return 1;
  fi;
  return (163 * data[1].func(NrFloatingBlocks(x), data[1].data)
          + data[2].func(UnderlyingBipartition(x), data[2].data))
          mod data[3] + 1;
end;

InstallMethod(ChooseHashFunction, "for a twisted bipartition",
[IsTwistedBipartition, IsInt],
function(x, hashlen)
  local data;
  data := [ChooseHashFunction(NrFloatingBlocks(x), hashlen),
           ChooseHashFunction(UnderlyingBipartition(x), hashlen),
           hashlen];
  return rec(func := SEMIGROUPS.TwistedBipartitionHashFunc,
             data := data);
end);

# Fundamental attributes

InstallMethod(DegreeOfBipartition, "for a twisted bipartition",
[IsTwistedBipartition], x -> DegreeOfBipartition(UnderlyingBipartition(x)));

InstallMethod(NrBlocks, "for a twisted bipartition",
[IsTwistedBipartition], x -> NrBlocks(UnderlyingBipartition(x)));

InstallMethod(NrLeftBlocks, "for a twisted bipartition",
[IsTwistedBipartition], x -> NrLeftBlocks(UnderlyingBipartition(x)));

InstallMethod(NrRightBlocks, "for a twisted bipartition",
[IsTwistedBipartition], x -> NrRightBlocks(UnderlyingBipartition(x)));

InstallMethod(RankOfBipartition, "for a twisted bipartition",
[IsTwistedBipartition], x -> RankOfBipartition(UnderlyingBipartition(x)));

# Attributes

InstallMethod(DomainOfBipartition, "for a twisted bipartition",
[IsTwistedBipartition], x -> DomainOfBipartition(UnderlyingBipartition(x)));
InstallMethod(CodomainOfBipartition, "for a twisted bipartition",
[IsTwistedBipartition], x -> DomainOfBipartition(UnderlyingBipartition(x)));