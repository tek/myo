{
  description = "Neovim Layout and Command Manager";

  inputs = {
    ribosome.url = "git+https://git.tryp.io/tek/ribosome";
  };

  outputs = {ribosome, ...}: ribosome.lib.pro ({config, ...}: {
    depsFull = [ribosome];
    compat.enable = false;
    hackage.versionFile = "ops/version.nix";
    gen-overrides.enable = true;

    cabal = {
      license = "BSD-2-Clause-Patent";
      license-file = "LICENSE";
      author = "Torsten Schmits";
      meta = {
        maintainer = "hackage@tryp.io";
        category = "Neovim";
        github = "tek/myo";
        extra-source-files = ["readme.md" "changelog.md"];
      };
      ghc-options = ["-fplugin=Polysemy.Plugin"];
      prelude = {
        enable = true;
        package = {
          name = "prelate";
          version = "^>= 0.6";
        };
        module = "Prelate";
      };
      dependencies = ["polysemy" "polysemy-plugin"];
    };

    buildInputs = pkgs: [pkgs.neovim pkgs.tmux pkgs.xterm pkgs.socat];

    packages.myo = {
      src = ./packages/myo;

      cabal.meta.synopsis = "Neovim Layout and Command Manager";

      library = {
        enable = true;
        dependencies = [
          "attoparsec"
          "chiasma"
          "chronos"
          "exon"
          "extra"
          "generic-lens"
          "hashable"
          "lens"
          "lens-regex-pcre"
          "messagepack"
          "microlens-mtl"
          "mono-traversable"
          "network"
          "optparse-applicative"
          "parsers"
          "path"
          "path-io"
          "pcre-light"
          "polysemy-chronos"
          "polysemy-process"
          "prettyprinter"
          "raw-strings-qq"
          "ribosome"
          "ribosome-host"
          "ribosome-menu"
          "template-haskell"
          "transformers"
          "typed-process"
          "unix"
          "uuid"
          "vector"
        ];
      };

      executable.enable = true;

    };

    packages.myo-test = {
      src = ./packages/test;

      cabal.meta.synopsis = "Myo tests";

      test = {
        enable = true;
        dependencies = [
          "chiasma"
          "exon"
          "hedgehog"
          "lens"
          "lens-regex-pcre"
          "myo"
          "path"
          "polysemy-chronos"
          "polysemy-process"
          "polysemy-test"
          "ribosome"
          "ribosome-host"
          "ribosome-menu"
          "ribosome-test"
          "tasty"
          "vector"
        ];
      };

    };

    main = "myo";
    exe = "myo";
    branch = "main";
    githubOrg = "tek";
    cachixName = "tek";
    cachixKey = "tek.cachix.org-1:+sdc73WFq8aEKnrVv5j/kuhmnW2hQJuqdPJF5SnaCBk=";

    envs.dev.buildInputs = with config.pkgs; [pkgs.neovim pkgs.tmux pkgs.socat];

    outputs.apps.myo = {
      type = "app";
      program =
        let main = "${config.outputs.packages.myo}/bin/myo --socat ${config.pkgs.socat}/bin/socat";
        in "${config.pkgs.writeScript "myo" main}";
    };

  });
}
