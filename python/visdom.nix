{ lib
, buildPythonPackage
, fetchPypi
, numpy
, scipy
, requests
, tornado
, pyzmq
, six
, jsonpatch
, websocket-client
, torchfile
, stdenv
, tree
, fetchurl
, pkgs
}:

with lib;
with strings;
with builtins;

let
  bootstrap = stdenv.mkDerivation rec {
    pname = "bootstrap";
    version = "3.3.7";
    src = fetchTarball {
      url = "https://github.com/twbs/bootstrap/archive/v3.3.7.tar.gz";
      sha256 = "0li7vdr4avz34b9xvwk7skbvnvzbx002cw5nfm7iwvi1wk8v5bri";
    };
    installPhase = ''
      mkdir -p $out
      cp -r . $out
    '';
  };

  urls = import ./visdom-deps.nix;
  fetchurl' = fetchurl;
  # fetchurl' = args: pkgs.lib.overrideDerivation
  #   (fetchurl (args // {
  #     sha256 = hashString "sha256" args.url;
  #     # downloadToTemp = true;
  #     # postFetch = ''
  #     #   export DOWNLOADED_FILE="$downloadedFile"
  #     #   export FETCH_OUT="$out"
  #     #   ${pkgs.runCommand "sha256sum" {
  #     #     # downloadedFile = "$DOWNLOADED_FILE";
  #     #     buildInputs = [ pkgs.nix ];
  #     #   } ''
  #     #       echo nix-hash --flat --base32 --type sha256 \$downloadedFile > $out
  #     #       echo "$FETCH_OUT"
  #     #   ''}'';
  #   }))
  #   (drv: {
  #     sha256 = drv.postFetch;
  #     downloadToTemp = false;
  #     # postFetch = ''
  #     #   echo Second
  #     # '';
  #     # postFetch = null;
  #     # downloadToTemp = true;
  #     # outputHashAlgo = null;
  #     # outputHash = null;
  #     # preferHashedMirrors = false;
    # });

  mkDerivation = name: meta: fetchurl' { inherit name; url = head meta; sha256 = head (tail meta); };
  derivations = attrValues (mapAttrs mkDerivation urls);
  link = drv: dst: ''cp -r "${drv}" "$STATIC/${dst}"'';
  link' = {name,outPath,...}:
    let
      dst = seq outPath
        (if hasSuffix ".css" name
         then "css"
         else "js");
      out = link (toString outPath) "${dst}/${name}";
    in seq out out;

in buildPythonPackage rec {
  pname = "visdom";
  version = "0.1.8.9";

  src = fetchPypi {
    inherit pname version;
    sha256 = "09kiczx2i5asqsv214fz7sx8wlyldgbqvxwrd0alhjn24cvx4fn7";
  };

  propagatedBuildInputs = [
    numpy
    scipy
    requests
    tornado
    pyzmq
    six
    jsonpatch
    websocket-client
    torchfile
    bootstrap
  ];

  doCheck = false;
  patches = [ ./visdom.patch ];

  postInstall = ''
    export STATIC=$(find "$out" -type d -regextype egrep -regex ".*/site-packages/visdom/static")
    ln -s "${bootstrap}/fonts" "$STATIC/fonts"
    ln -s "${bootstrap}/js/*"    "$STATIC/js"
    ${concatMapStringsSep "\n" link' derivations}
  '';

  pythonImportsCheck = [ "visdom" ];

  meta = with lib; {
    description = "A flexible tool for creating, organizing, and sharing visualizations of live, rich data. Supports Torch and Numpy.";
    homepage = "https://github.com/fossasia/visdom";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
