with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "tomp4-converter-nbb";
  buildInputs = [ clojure-lsp clj-kondo jq pkgs.nodejs nodePackages.npm ffmpeg ];
}
