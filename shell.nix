let

  # Must be the same as the Stack resolver we use!
  ghcCompiler = "ghc8106";

  # By default, all functions/values come fron Nixpkgs
in
with import nix/default.nix { };
let
    # Wrap Stack to configure Nix integration and target the correct Stack-Nix file
  # --nix -> Enable Nix support
  # --no-nix-pure -> Pass environment variables (like LOCALES or FEDID_URL)
  # --nix-shell-file nix/stack-integration.nix -> Specify the Nix file to use (otherwise it uses shell.nix by default, which we don't want)
  stack-wrapped = symlinkJoin {
    name = "stack";
    paths = [ stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file nix/stack-integration.nix \
        "
    '';
  };

in
mkShell {

  # Useful packages for One Catalog, you can manually use `nix-shell` to get a shell with everything configured, or even better, install `nix-direnv`, see `.envrc`
  buildInputs = [
    stack

    # Development tools #
    # Formatter
    ormolu
    # Code suggestions
    hlint
    # Haddock parsing and search

  ];

}
