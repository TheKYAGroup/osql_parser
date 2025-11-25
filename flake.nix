{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    naersk.url = "github:nix-community/naersk";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nixpkgs-mozilla = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };
  };

  outputs =
    {
      self,
      flake-utils,
      naersk,
      nixpkgs,
      nixpkgs-mozilla,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = (import nixpkgs) {
          config.allowUnfree = true;
          inherit system;
          overlays = [ (import nixpkgs-mozilla) ];
        };

        toolchain =
          (pkgs.rustChannelOf {
            rustToolchain = ./rust-toolchain;
            sha256 = "sha256-3ZSJYswbGJ/c8dSUEuDPJD1D1SCgGjHlg97gF8LtbAU=";
          }).rust;

        naersk' = pkgs.callPackage naersk {
          cargo = toolchain;
          rustc = toolchain;
        };

        buildPackageFor =
          releaseMode:
          naersk'.buildPackage rec {
            src = ./.;
            release = releaseMode;
            nativeBuildInputs = with pkgs; [ makeWrapper ];
            postInstall = ''
              		for prog in $out/bin/*; do
              			wrapProgram $prog --set LD_LIBRARY_PATH ${pkgs.lib.makeLibraryPath nativeBuildInputs}
              		done
              	  '';
          };

      in
      rec {
        # For `nix build` & `nix run`:
        defaultPackage = debugPackage;

        # Debug build
        packages.debug = debugPackage;
        # Release mode
        packages.release = releasePackage;

        debugPackage = buildPackageFor false;
        releasePackage = buildPackageFor true;

        # For `nix develop`:
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            rustc
            cargo
            openssl_3
          ];
        };
      }
    );
}
