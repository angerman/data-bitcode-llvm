# Data.BitCode.LLVM

A module to produce bitcode from llvm module descriptions.

# TODO

## High prio
- [ ] Extend the call instruction to carry the symbol, signature
      and calling conv, the call is performed with, and verify
      that the symbols type matches the signature; and calling conv
      if symbol is a Function Symbol.  If Symbol is a reference
      ensure that the signature matches. (Data.BitCode.LLVM should
      automatically insert the required ptr cast if the symbol does
      not match).
      Also error if neither a Reference with matching signature nor
      a Function. I don't think calling anything else makes any
      sense?
- [ ] Drop the stupid Type first item from the instruction records.
      This should be computed using `instTy`.
- [ ] Make `instTy` work for `getElementPointer`.
- [ ] Write tests to verify that `instTy` works as expected.
- [ ] Verify, e.g. in `mkInstRec` for `Call` (e.g. everywhere where we
      do not use `instTy` that the type we encode matches the one
      we should expect.
- [ ] Fix getValue. We currently *assume* that we have no fwd references.
      And hence can always load the value, but llvm does not work that
      way for fwd references.  There the value and type are encoded after
      each other (ValueId, TypeId). If it's non fwd reference, it's
      just ValueId. (see getValueTypePair in BitCodeReader). Similarly this
      is required to support fwd references in ToBitCode.
- [x] Collapse constants.

## General
- [ ] Stop handrolling your monads. (Use transformers, and derive!)
- [ ] Better error reporting (e.g. see preliminaries in `Data.BitCode.LLVM.Util`)
      Maybe using `ExceptT` to give good hints as to what faild?
- [ ] More type verification. (E.g. let's try to make sure you 
      just can not construct invalid code)
