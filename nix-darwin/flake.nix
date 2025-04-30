{
  description = "BKR nix-darwin system";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs }:
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#deucalion
    darwinConfigurations."deucalion" = nix-darwin.lib.darwinSystem {
      specialArgs = { inherit self; };
      modules = [ ./darwin.nix ];
    };
  };
}
