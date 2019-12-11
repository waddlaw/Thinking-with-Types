# ch13

```
λ cabal build jsonschema
Build profile: -w ghc-8.6.5 -O1
In order, the following will be built (use -v for more details):
 - ch13-0.1.0.0 (lib) (first run)
Preprocessing library for ch13-0.1.0.0..
Building library for ch13-0.1.0.0..
[1 of 2] Compiling JSONSchema       ( JSONSchema.hs, /Users/gupi/Desktop/repos/wip-book/twt/ch13/dist-newstyle/build/x86_64-osx/ghc-8.6.5/ch13-0.1.0.0/build/JSONSchema.o )
[2 of 2] Compiling InspectionTesting ( InspectionTesting.hs, /Users/gupi/Desktop/repos/wip-book/twt/ch13/dist-newstyle/build/x86_64-osx/ghc-8.6.5/ch13-0.1.0.0/build/InspectionTesting.o )
InspectionTesting.hs:14:1: mySchema mentions none of GHC.Generics.V1, GHC.Generics.U1, GHC.Generics.M1, GHC.Generics.K1, GHC.Generics.:+:, GHC.Generics.:*:, GHC.Generics.:.:, GHC.Generics.Rec1, GHC.Generics.Par1 passed.
inspection testing successful
      expected successes: 1
```