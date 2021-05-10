{
  description = "Neovim Layout and Command Manager";

  inputs.ribosome.url = github:tek/ribosome;

  outputs = { ribosome, ... }:
  let
    inherit (ribosome.inputs) chiasma;
    inherit (chiasma.inputs) hix;
    overrides = { hackage, source, minimal, configure, pkgs, ... }: {
      cornea = hackage "0.4.0.0" "1w9rkf6f861kknkskywb8fczlk7az8m56i3hvmg6a5inpvqf6p7i";
      chiasma = source.package chiasma "chiasma";
      myo-test = drv: drv.overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [pkgs.neovim pkgs.tmux pkgs.rxvt-unicode];
      });
      ribosome = configure "--extra-prog-path=${pkgs.neovim}/bin" (minimal (source.package ribosome "ribosome"));
      ribosome-test = minimal (source.package ribosome "test");
    };

  in hix.flake {
    base = ./.;
    inherit overrides;
    compat = false;
    packages = {
      myo = ./packages/myo;
      myo-test = ./packages/test;
    };
    main = "myo";
    versionFile = "ops/hpack/packages/meta.yaml";
    runConfig = p: { extraShellInputs = [p.pkgs.neovim]; };
    modify = _: outputs: rec {
      apps.myo = {
        type = "app";
        program = "${outputs.packages.myo}/bin/myo";
      };
      defaultApp = apps.myo;
    };
  };
}
