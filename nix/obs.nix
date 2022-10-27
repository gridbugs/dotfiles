{ pkgs ? import <nixpkgs> { } }:

{
  obs = pkgs.wrapOBS {
    plugins = [
      pkgs.obs-studio-plugins.obs-backgroundremoval
    ];
  };
}
