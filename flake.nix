{
  description = "monolog";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              monolog = hfinal.callCabal2nix "monolog" ./. { };
            };
        };
        monolog = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.monolog;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = monolog-shell;
            monolog-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.monolog ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = monolog;
            monolog = pkgs.monolog;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
