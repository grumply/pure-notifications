{ mkDerivation, stdenv, ghc, base, pure-elm, pure-lifted, 
  pure-render, pure-time, pure-txt, pure-websocket
}:
mkDerivation {
  pname = "pure-notifications";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    pure-elm
    pure-lifted
    pure-render
    pure-time
    pure-txt
    pure-websocket
  ];
  homepage = "github.com/grumply/pure-notifications";
  license = stdenv.lib.licenses.bsd3;
}
