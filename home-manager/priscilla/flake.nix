{
  description = "Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = { nixpkgs, home-manager, neovim-nightly-overlay, ... }:
    let
      system = "aarch64-darwin";
      pkgs = import nixpkgs {
        system = system;
        config = { allowUnfree = true; };
      };
    in {
      homeConfigurations.s = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        modules = [ ./home.nix ];

        extraSpecialArgs = {
          extraPkgs =
            [ neovim-nightly-overlay.packages.${pkgs.system}.default ];
        };
      };
    };
}
