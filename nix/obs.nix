{ pkgs ? import <nixpkgs> { } }:

{
  obs = pkgs.wrapOBS {
    plugins = [
    ];
  };
}
