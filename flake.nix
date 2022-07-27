{
  description = "Neovim Layout and Command Manager";

  inputs.ribosome.url = git+https://git.tryp.io/tek/ribosome;

  outputs = { ribosome, ... }:
  let

    overrides = { buildInputs, fast, pkgs, source, ... }:
    let
      inputs = buildInputs [pkgs.neovim pkgs.tmux pkgs.xterm];
    in {
      myo = buildInputs [pkgs.socat] fast;
      myo-test = fast inputs;
    };

  in ribosome.lib.flake ({ config, lib, ...}: {
    base = ./.;
    inherit overrides;
    depsFull = [ribosome];
    compat.enable = false;
    packages = {
      myo = ./packages/myo;
      myo-test = ./packages/test;
    };
    main = "myo";
    exe = "myo";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";
    hpack = {
      packages = import ./ops/hpack.nix { inherit config lib; };
      defaultApp = "myo";
    };
    hackage.versionFile = "ops/version.nix";
    ghcid.shellConfig.buildInputs = with config.devGhc.pkgs; [pkgs.neovim pkgs.tmux pkgs.socat];
    ghci = {
      preludePackage = "prelate";
      preludeModule = "Prelate";
      args = ["-fplugin=Polysemy.Plugin"];
      extensions = ["StandaloneKindSignatures" "OverloadedLabels" "ImpredicativeTypes"];
    };
  });
}
