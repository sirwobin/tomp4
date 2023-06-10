with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "tomp4-converter-nbb";
  buildInputs = [ jq pkgs.nodejs nodePackages.npm ffmpeg ];
}
