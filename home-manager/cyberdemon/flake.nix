{
  description = "Home Manager configuration";

  inputs = {
    # Specify the source of Home Manager and Nixpkgs.
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
  };

  outputs = { nixpkgs, home-manager, neovim-nightly-overlay, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { system = system; config = { allowUnfree = true; }; };
    in {
      homeConfigurations.s = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [ ./home.nix ];

        extraSpecialArgs = {
          extraPkgs = [
             neovim-nightly-overlay.packages.${pkgs.system}.default
           ];
        };
      };
    };
}