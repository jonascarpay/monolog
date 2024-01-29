{
  description = "slog";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              slog = hfinal.callCabal2nix "slog" ./. { };
            };
        };
        slog = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.slog;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = slog-shell;
            slog-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.slog ];
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
            default = slog;
            slog = pkgs.slog;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
