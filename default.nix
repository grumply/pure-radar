{ mkDerivation, base, pure, pure-css, pure-prop, pure-svg
, pure-theme, pure-txt, stdenv
}:
mkDerivation {
  pname = "pure-radar";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base pure pure-css pure-prop pure-svg pure-theme pure-txt
  ];
  homepage = "github.com/grumply/pure-radar";
  description = "SVG radar charts";
  license = stdenv.lib.licenses.bsd3;
}
