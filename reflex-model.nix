{ mkDerivation, base, containers, lens, reflex, stdenv }:
mkDerivation {
  pname = "reflex-model";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers lens reflex ];
  license = stdenv.lib.licenses.bsd3;
}
