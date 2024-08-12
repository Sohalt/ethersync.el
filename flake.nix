{
  inputs.ethersync.url = "github:ethersync/ethersync";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {
    nixpkgs,
    flake-utils,
    ethersync,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.default = pkgs.mkShell {
        packages = let ep = ethersync.packages.${system}; in [ep.ethersync ep.neovim];
      };
    });
}
