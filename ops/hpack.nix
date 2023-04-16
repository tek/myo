{ config, lib, ... }:
with builtins;
with lib;
let

in {

  myo = merge (project "myo") {
    library.dependencies = [
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
      ];
    };
  };

}
