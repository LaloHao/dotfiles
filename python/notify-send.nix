{ lib
, buildPythonPackage
, fetchFromGitHub
, dbus-python
, pygobject3
, notify2
, fetchPypi
, wrapPython
, stdenv
, python
, gobjectIntrospection
, makeWrapper
}:

stdenv.mkDerivation rec {
  pname = "notify-send.py";
  version = "1.2.7";
  src = fetchPypi {
    inherit pname version;
    sha256 = "0qwn7ln655aazm7pvz3fzvm5fmknjxg6m53ahisnrmkakx25k2gn";
  };
  # nativeBuildInputs = [ wrapPython ];
  buildInputs = [ makeWrapper ];
  installPhase = ''
    runHook preInstall
    mkdir -p $out/{bin,lib}
    cp -r notify_send_py/* $out/lib
    runHook postInstall
  '';
  propagatedBuildInputs = [
    dbus-python
    pygobject3
    notify2
  ];

  postInstall = ''
    substituteInPlace $out/lib/notify_send_py.py \
      --replace ".notify3" "notify3"
    makeWrapper $out/lib/notify_send_py.py $out/bin/notify-send.py
  '';
  # postFixup = "wrapPythonPrograms";
}
