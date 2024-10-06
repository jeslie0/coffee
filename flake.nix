{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    ps-overlay.url = "github:thomashoneyman/purescript-overlay";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    haskellNix = {
      url = "github:jeslie0/haskell.nix";
      # inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, ps-overlay, mkSpagoDerivation, haskellNix }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [ haskellNix.overlay
                       ps-overlay.overlays.default
                       mkSpagoDerivation.overlays.default
                     ];
        });

      ghcVersion =
        "ghc965";

      extendHaskellPackages = { haskellPackages }:
        haskellPackages.extend ( hpFinal: hpPrev: {});

      haskellPackages = system:
        extendHaskellPackages {
          haskellPackages =
            nixpkgsFor.${system}.haskell.packages.${ghcVersion};
        };

      packageName = system: with builtins;
        let
          cabalFileName =
            let
              cabalFiles =
                ((filter ((nixpkgsFor.${system}).lib.hasSuffix ".cabal")) (attrNames (readDir ./.)));
            in
              head cabalFiles;

          matches =
            (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./.}\/${cabalFileName}"));
        in
          head matches;
    in
      {
        packages =
          forAllSystems (system:
            {
              nixpkgs =
                nixpkgsFor.${system};

              default =
                (haskellPackages system).callCabal2nix (packageName system) self {};
              web = import ./nix/web.nix { inherit self; pkgs = nixpkgsFor.${system}; };
            } // (
              import ./nix/xCompiled.nix {
                inherit ghcVersion system nixpkgs extendHaskellPackages self;

                pkgs =
                  nixpkgsFor.${system};

                packageName =
                  packageName system;
              })
          );

        devShell =
          forAllSystems (system:
            let
              pkgs =
                nixpkgsFor.${system};
            in
              (haskellPackages system).shellFor {
                # The packages that the shell is for.
                packages = p: [
                  self.packages.${system}.default
                ];

                buildInputs = with (haskellPackages system);
                  [  haskell-language-server
                     pkgs.purescript-language-server-unstable
                     cabal-install
                     pkgs.spago-unstable
                     pkgs.purs-unstable
                     pkgs.nodePackages.npm
                     pkgs.nodejs
                     pkgs.purs-tidy
                  ];


                # Add build inputs of the following derivations.
                inputsFrom = [ ];

                # Enables Hoogle for the builtin packages.
                withHoogle = true;
              }
          );
      };
}
