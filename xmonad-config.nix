{ mkDerivation, base, containers, lens, mtl, process, stdenv
, transformers, unix, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "my-xmonad";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers lens mtl process transformers unix xmonad
    xmonad-contrib
  ];
  license = stdenv.lib.licenses.bsd3;
}
