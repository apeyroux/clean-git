{ mkDerivation, base, optparse-applicative, stdenv, text }:
mkDerivation {
  pname = "clean-git";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base optparse-applicative text ];
  homepage = "https://github.com/githubuser/clean-git#readme";
  license = stdenv.lib.licenses.bsd3;
}
