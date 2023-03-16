{ self, ... }:

let
  imageName = "ghcr.io/nammayatri/nammayatri";
  # self.rev will be non-null only when the working tree is clean
  # This is equivalent to `git rev-parse --short HEAD`
  imageTag = builtins.substring 0 9 (self.rev or "dev");
in
{
  config = {
    flake.dockerImageName = imageName + ":" + imageTag;
    perSystem = { self', pkgs, lib, ... }: {
      packages = lib.optionalAttrs pkgs.stdenv.isLinux {
        dockerImage =
          # TODO: Build a layered image, separating tools and packages
          pkgs.dockerTools.buildImage {
            name = imageName;
            created = "now";
            tag = imageTag;
            copyToRoot = pkgs.buildEnv {
              paths = with pkgs; [
                cacert
                self'.packages.default
                awscli
                coreutils
                bash
                # Add project root to paths to copy dhall-configs and swagger dirs
                self
              ];
              name = "beckn-root";
              pathsToLink = [ "/dhall-configs" "/swagger" "/bin" ];
            };
            config = {
              Env = [ "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt" ];
              Cmd = [ "${pkgs.lib.getExe self'.packages.default}" ];
            };
          };
      };
    };
  };
}