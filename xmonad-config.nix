{ mkDerivation, base, lens, stdenv, unix, xmonad, xmonad-contrib }:
mkDerivation {
  pname = "xmonad-config";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens unix xmonad xmonad-contrib ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
