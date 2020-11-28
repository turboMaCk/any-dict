{ pkgs ? import <nixpkgs> {} }:
with pkgs;
mkShell {
  name = "any-dict-shell";
  buildInputs = with elmPackages;
    [ elm
      elm-test
      elm-verify-examples
      elm-format
    ];
}
