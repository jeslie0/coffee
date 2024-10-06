{self, pkgs, system}:
let spago =
      pkgs.mkSpagoDerivation {
        spagoYaml = "${self}/web/spago.yaml";
        spagoLock = "${self}/web/spago.lock";
        src = "${self}/web";
        nativeBuildInputs = [ pkgs.purs-unstable pkgs.spago-unstable pkgs.esbuild];
        version = "0.1.0";
        buildPhase = "spago build";
        installPhase = "mkdir $out; cp -r * $out";
      };

    npmPkg =
      pkgs.buildNpmPackage {
        pname = "coffee";
        version = "0.1.0";
        src = spago;
        npmDepsHash = "sha256-zEtMjnOjqtA+1b1gnkt6NeAzPuOOVj7/262+bWLLfbw=";
      };
in
pkgs.stdenv.mkDerivation {
  pname = "coffee";
  version = "0.1.0";
  src = npmPkg;
  installPhase = "mkdir $out; cp -r lib/node_modules/assistant-pi/dist/* $out";
}
