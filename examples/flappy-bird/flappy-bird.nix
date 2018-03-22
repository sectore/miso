{ mkDerivation, base, containers, random, miso, stdenv }:
mkDerivation {
  pname = "miso-flappy-bird";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers random miso ];
  description = "Miso flappy bird example";
  license = stdenv.lib.licenses.bsd3;
  postInstall = ''
    mkdir -p $out/bin/flappy-bird.jsexe/images
    cp -r ./images $out/bin/flappy-bird.jsexe/
    cp ./index.html $out/bin/flappy-bird.jsexe/
  '';
}
