## TODO

delete me later.

## Important Places

- **compiler/types/TyCon.hs** is where "entities" are declared. These
  are the more mature representation of all type constructors in
  the typechecking phase. I have added `wfChild :: Maybe TyCon`
  to the FamilyTyCon constructor to represent the associated
  constraint family for each type family instance, e.g,
  if `F [a] = Tree a` then wfChild will link to `WF_F` s.t.
  `WF_F [a] = Tree @ a`.
- **compiler/typecheck/TcTyClsDecls.hs** is where all type constructors
  are type checked. Apoorv has added his WF augmentation into
  function `tcTyClGroup`, which type checks one strongly-connected
  component. Will want to introduce a new type family *here*.
- **compiler/prelude/TysWiredIn.hs** is where Apoorv wires-in the
  `@@` type constraint. So *some* of this can be used as a model
  for how to create additional type families. The only difference being
  that I will need to be inside a Monad to generate Unique IDs.
- **compiler/typecheck/TcTyWF.hs** is all of the well-formededness
  logic Apoorv has added. At some point I will want to reuse this logic
  to generate the additional WF constraint instances for each `WfChild` typelass.
  
## Steps

### Adding type families

1. The first thing to do is just try to get the type families in
   `TcTyClsDecls.hs` and ~~shit~~ print them out. 
2. Once we have the type families, populate `WfChild` with something trivial -- maybe
   the `All` type family (it doesn't matter what's here).
3. Now print / trace and see that these new TF's are in scope.
4. Now worry about creating an appropriately named `WfChild` TF. See that it's
   added in tests.
   
## Populating WF constraints on type instances

Worry about this later.
