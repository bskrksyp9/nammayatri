{
  inputs = {
    common.url = "github:nammayatri/common";
    nixpkgs.follows = "common/nixpkgs";
    flake-parts.follows = "common/flake-parts";
    systems.url = "github:nix-systems/default";

    # Backend inputs
    shared-kernel.url = "github:nammayatri/shared-kernel/34e75caece0a6b3c8d119a0a904efc6f41e0390b";
    shared-kernel.inputs.nixpkgs.follows = "nixpkgs";
    beckn-gateway.url = "github:nammayatri/beckn-gateway";
    beckn-gateway.inputs.shared-kernel.follows = "shared-kernel";
  };
  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.common.flakeModules.default
        ./Backend/default.nix
        ./Frontend/default.nix
      ];
      perSystem = { config, self', pkgs, ... }: {
        packages.default = self'.packages.nammayatri;

        devShells.default = pkgs.mkShell {
          # cf. https://haskell.flake.page/devshell#composing-devshells
          inputsFrom = [
            config.mission-control.devShell
            config.pre-commit.devShell
            config.flake-root.devShell
            config.haskellProjects.default.outputs.devShell
          ];
        };
      };
    };
}
