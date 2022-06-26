{ config, lib, ... }:
with builtins;
with lib;
let

  mergeAttr = a: b:
  if isAttrs a
  then merge a b
  else if isList a
  then a ++ b
  else b;

  merge = l: r:
  let
    f = name:
    if hasAttr name l && hasAttr name r
    then mergeAttr l.${name} r.${name}
    else l.${name} or r.${name};
  in genAttrs (concatMap attrNames [l r]) f;

  paths = name: {
    when = {
      condition = false;
      generated-other-modules = ["Paths_${replaceStrings ["-"] ["_"] name}"];
    };
  };

  meta = {
    version = "0.1.0.0";
    license = "BSD-2-Clause-Patent";
    license-file = "LICENSE";
    author = "Torsten Schmits";
    maintainer = "hackage@tryp.io";
    copyright = "2022 Torsten Schmits";
    category = "Neovim";
    build-type = "Simple";
    github = "tek/myo";
  };

  options.ghc-options = [
    "-Wall"
    "-Wredundant-constraints"
    "-Wincomplete-uni-patterns"
    "-Wmissing-deriving-strategies"
    "-Widentities"
    "-Wunused-packages"
    "-fplugin=Polysemy.Plugin"
  ];

  dependencies = [
      { name = "base"; version = ">= 4.12 && < 5"; mixin = "hiding (Prelude)"; }
      { name = "prelate"; version = ">= 0.1"; mixin = ["(Prelate as Prelude)" "hiding (Prelate)"]; }
      "polysemy"
      "polysemy-plugin"
    ];

  basic = name: merge (meta // options) {
    inherit name;
    default-extensions = config.ghci.extensions;
  };

  project = name: basic name // {
    library = paths name // {
      source-dirs = "lib";
      inherit dependencies;
    };
  };

  exe = name: dir: merge (paths name // {
    main = "Main.hs";
    source-dirs = dir;
    inherit dependencies;
    ghc-options = [
      "-threaded"
      "-rtsopts"
      "-with-rtsopts=-N"
    ];
  });

in {

  myo = merge (project "myo") {
    synopsis = "Neovim Layout and Command Manager";
    description = "See https://hackage.haskell.org/package/myo/docs/Myo.html";
    library.dependencies = [
      "Glob"
      "aeson"
      "attoparsec"
      "chiasma"
      "chronos"
      "exon"
      "extra"
      "filepattern"
      "first-class-families"
      "foldl"
      "generic-lens"
      "generics-sop"
      "hashable"
      "lens"
      "lens-regex-pcre"
      "messagepack"
      "network"
      "parsers"
      "path"
      "path-io"
      "pcre-light"
      "polysemy-chronos"
      "polysemy-process"
      "polysemy-time"
      "prettyprinter"
      "raw-strings-qq"
      "ribosome"
      "ribosome-host"
      "ribosome-menu"
      "stm-chans"
      "streamly"
      "string-interpolate"
      "stringsearch"
      "transformers"
      "typed-process"
      "unix"
      "uuid"
      "vector"
    ];
    executables.myo = exe "myo" "app" {
      dependencies = ["myo"];
    };
  };

  myo-test = merge (project "myo-test") {
    synopsis = "Myo tests";
    description = "See https://hackage.haskell.org/package/myo/docs/Myo.html";
    tests.myo-unit = exe "myo-test" "test" {
      dependencies = [
        "aeson"
        "chiasma"
        "chiasma-test"
        "exon"
        "extra"
        "hedgehog"
        "lens"
        "lens-regex-pcre"
        "myo"
        "path"
        "path-io"
        "pcre-heavy"
        "polysemy-chronos"
        "polysemy-process"
        "polysemy-test"
        "ribosome"
        "ribosome-host"
        "ribosome-menu"
        "ribosome-test"
        "streamly"
        "tasty"
        "unix"
        "vector"
      ];
    };
  };

}
