{
  description = "Neovim Layout and Command Manager";

  inputs.ribosome.url = github:tek/ribosome;

  outputs = { ribosome, ... }:
  let
    inherit (ribosome.inputs) chiasma hix;

    overrides = { hackage, source, minimal, configure, pkgs, transform_, ... }: {
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      chiasma = source.package chiasma "chiasma";
      myo-test = transform_ (drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.ripgrep pkgs.rxvt-unicode];
      }));
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" (minimal (source.package ribosome "ribosome"));
      ribosome-test = minimal (source.package ribosome "test");
    };

  in hix.lib.flake ({ config, ...}: {
    base = ./.;
    inherit overrides;
    deps = [ribosome];
    compat.enable = false;
    packages = {
      myo = ./packages/myo;
      myo-test = ./packages/test;
    };
    main = "myo";
    versionFile = "ops/hpack/packages/meta.yaml";
    ghcid.shellConfig =
    let pkgs = config.devGhc.pkgs;
    in { buildInputs = [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode]; };
    output.amend = _: outputs: rec {
      apps = rec {
        myo = {
          type = "app";
          program = "${outputs.packages.myo}/bin/myo";
        };
        default = myo;
      };
    };
  });
}
