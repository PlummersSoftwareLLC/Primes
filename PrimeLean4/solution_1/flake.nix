{
  description = "PrimeLean4";

  inputs.lean.url = github:leanprover/lean4;
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, nixpkgs, lean, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        leanPkgs = lean.packages.${system};
        primePkg = leanPkgs.buildLeanPackage {
          name = "PrimeLean4";  # must match the name of the top-level .lean file
          src = ./.;
        };
      in {

        defaultPackage = primePkg.modRoot;

        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            elan
            gmp
          ];
        };
      });
}
