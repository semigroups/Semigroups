

#this method shouldn't be necessary!
InstallMethod(IsBinaryRelationCollection, "for a collection",
[IsCollection], coll-> IsBinaryRelation(Representative(coll)));

InstallOtherMethod(IsBinaryRelation, "for a mult element",
[IsMultiplicativeElement], ReturnFalse);

