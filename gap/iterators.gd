


DeclareGlobalFunction("IteratorByNextIterator");
DeclareGlobalFunction("IteratorByIterOfIter");
DeclareGlobalFunction("IteratorByIterator");
DeclareGlobalFunction("ListByIterator");

DeclareOperation("IteratorOfDClasses", [IsSemigroup]);
DeclareOperation("IteratorOfHClasses", [IsSemigroup]);
DeclareOperation("IteratorOfLClasses", [IsSemigroup]); 
DeclareOperation("IteratorOfRClasses", [IsSemigroup]);

DeclareOperation("IteratorOfDClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfLClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfHClassReps", [IsSemigroup]); 
DeclareOperation("IteratorOfRClassReps", [IsSemigroup]);

DeclareOperation("IteratorOfRClassData", [IsSemigroup]);
DeclareOperation("IteratorOfDClassData", [IsSemigroup]);
DeclareOperation("IteratorOfHClassData", [IsSemigroup]);
DeclareOperation("IteratorOfLClassData", [IsSemigroup]);

DeclareProperty("IsIteratorOfRClassReps", IsIterator);
DeclareProperty("IsIteratorOfLClassReps", IsIterator);
DeclareProperty("IsIteratorOfDClassReps", IsIterator);
DeclareProperty("IsIteratorOfHClassReps", IsIterator);

DeclareProperty("IsIteratorOfRClasses", IsIterator);
DeclareProperty("IsIteratorOfLClasses", IsIterator);
DeclareProperty("IsIteratorOfDClasses", IsIterator);
DeclareProperty("IsIteratorOfHClasses", IsIterator);

DeclareProperty("IsIteratorOfSemigroup", IsIterator);
DeclareProperty("IsIteratorOfRClassElements", IsIterator);
DeclareProperty("IsIteratorOfLClassElements", IsIterator);
DeclareProperty("IsIteratorOfDClassElements", IsIterator);
DeclareProperty("IsIteratorOfHClassElements", IsIterator);




