{ mkDerivation, base, containers, miso, stdenv }:
mkDerivation {
  pname = "miso-flappy-bird";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers miso ];
  description = "Miso's flappy birds example";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    cp ./index.html $out/bin/flappy-bird.jsexe/
  '';
}
