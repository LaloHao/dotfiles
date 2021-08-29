{ pkgs ? import <nixpkgs> {  } }:
let
  mkDictFromRla = { shortName, dictFileName }:
    pkgs.hunspellDicts."${dictFileName}".overrideAttrs(o: rec {
      inherit dictFileName;
      version = "2.6";
      readmeFile = "README.txt";
      src = pkgs.fetchFromGitHub {
        owner = "sbosio";
        repo = "rla-es";
        sha256 = "1a3d2d2v579znb320fj7fw2j3v2khki3q7cjpq39nwlibv8bafjb";
        rev = "v${version}";
      };
      patchPhase = ''
        substituteInPlace herramientas/make_dict.sh \
         --replace /bin/bash "${pkgs.bash}/bin/bash"

        substituteInPlace herramientas/remover_comentarios.sh \
         --replace /bin/bash "${pkgs.bash}/bin/bash"

        cat <<EOF > .versiones.cfg
CORRECTOR="2.6"
SEPARACION="2.6"
SINONIMOS="28/08/2021"
LO_DICTIONARIES_GIT=""
EOF
      '';
      buildPhase = ''
        bash -xe herramientas/make_dict.sh -l ${dictFileName} <<<s
        cd productos
        unzip ${dictFileName}.oxt \
          ${dictFileName}.dic ${dictFileName}.aff ${readmeFile}
      '';
    });
in rec {
  inherit mkDictFromRla;

  es-mx = es_MX;
  es_MX    = mkDictFromRla {
    shortName = "es-mx";
    dictFileName = "es_MX";
  };
}
