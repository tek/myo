{
  description = "Neovim Layout and Command Manager";

  inputs.ribosome.url = git+https://git.tryp.io/tek/ribosome?ref=polysemy;
  inputs.prelate.url = git+https://git.tryp.io/tek/prelate;

  outputs = { ribosome, prelate, ... }:
  let
    inherit (ribosome.inputs) hix;

    overrides = { buildInputs, fast, pkgs, source, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      myo = fast;
      myo-test = fast inputs;
      prelate = source.package prelate "prelate";
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
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels" "ImpredicativeTypes"];
    };
  });
}
