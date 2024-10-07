{self, pkgs}:
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
        npmDepsHash = "sha256-hK9y/OImxgNUNIMpCK3ksFVZyS03i+GqdGZEiG/E3mM=";
      };
in
pkgs.stdenv.mkDerivation {
  pname = "coffee";
  version = "0.1.0";
  src = npmPkg;
  installPhase = "mkdir $out; cp -r lib/node_modules/coffee-log/dist/* $out";
}
