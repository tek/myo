{
  description = "Neovim Layout and Command Manager";

  inputs.ribosome.url = git+https://git.tryp.io/tek/ribosome?ref=polysemy;

  outputs = { ribosome, ... }:
  let
    inherit (ribosome.inputs) hix;

    overrides = { buildInputs, fast, pkgs, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      myo = fast;
      myo-test = fast inputs;
    };

  in hix.lib.flake ({ config, lib, ...}: {
    base = ./.;
    inherit overrides;
    depsFull = [ribosome];
    compat.enable = false;
    packages = {
      myo = ./packages/myo;
      myo-test = ./packages/test;
    };
    main = "myo-test";
    hpack = {
      packages = import ./ops/hpack.nix { inherit config lib; };
      defaultApp = "proteome";
    };
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux];
    ghci = {
      preludePackage = "incipit";
      preludeModule = "Incipit";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels" "ImpredicativeTypes"];
    };
  });
}
