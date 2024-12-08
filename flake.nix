{
  description = "Advent of code";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    fourmolu-nix.url = "github:jedimahdi/fourmolu-nix";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.fourmolu-nix.flakeModule
      ];
      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./src
              ./aoc.cabal
            ];
          });
          # The base package set (this value is the default)
          # basePackages = pkgs.haskellPackages;

          # Packages to add on top of `basePackages`
          packages = {
            #happy.source = "1.20.1.1";
          };

          # Add your package overrides here
          settings = {
            # Need alex and happy
            compiler = {
              # extraBuildDepends = [ pkgs.haskellPackages.alex pkgs.haskellPackages.happy ];
            };
          };

          # Development shell configuration
          devShell = {
            hlsCheck.enable = true;
            mkShellArgs = {
              # nativeBuildInputs = with pkgs; [
              #   haskellPackages.happy
              #   haskellPackages.alex
              # ];
            };
            # need happy and alex
          };

          
              # settings.haskell-language-server.custom = with pkgs.haskell.lib.compose; lib.flip lib.pipe [
              #   (disableCabalFlag "ormolu")
              #   (disableCabalFlag "fourmolu")
              #   (builtins.trace "hls override" (drv: drv.override { hls-ormolu-plugin = null; }))
              #   (drv: drv.override { hls-fourmolu-plugin = null; })
              # ];

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs.fourmolu = {
            enable = true;
            package = config.fourmolu.wrapper;
          };
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;
        };

        fourmolu.settings = {
          indentation = 2;
          comma-style = "leading";
          record-brace-space = true;
          # haskellPackages.
          indent-wheres = true;
          import-export-style = "diff-friendly";
          respectful = true;
          haddock-style = "multi-line";
          newlines-between-decls = 1;
          extensions = [ "ImportQualifiedPost" ];
        };

        # Default package & app.
        packages.default = self'.packages.aoc;
        apps.default = self'.apps.aoc;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "compiler";
          meta.description = "Haskell development environment";
          # See https://community.flake.parts/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.treefmt.build.devShell
          ];
          nativeBuildInputs = with pkgs; [
            just
            csmith
            gcc
            haskellPackages.fast-tags
          ];
        };
      };
    };
}
