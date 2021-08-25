{ pkgs, env }:

pkgs.jupyter.override {
  definitions = {
    python3 = {
      displayName = "Python 3";
      argv = [
        "${env.interpreter}"
        "-m"
        "ipykernel_launcher"
        "-f"
        "{connection_file}"
      ];
      language = "python";
      logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
      logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
    };
  };
}
