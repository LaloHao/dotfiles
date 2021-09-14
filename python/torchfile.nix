{ lib
, buildPythonPackage
, fetchFromGitHub
, numpy
, pillow
}:

buildPythonPackage rec {
  pname = "torchfile";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "bshillingford";
    repo = "python-torchfile";
    rev = version;
    sha256 = "0ffajg536jhm772k0kcm3m721rq6j6ldxfjz0hmad0x85dhcr7v2";
  };

  propagatedBuildInputs = [
    numpy
    pillow
  ];

  doCheck = false;

  pythonImportsCheck = [ "torchfile" ];

  meta = with lib; {
    description = "Deserialize Lua torch-serialized objects from Python";
    homepage = "https://github.com/bshillingford/python-torchfile";
    license = licenses.free;
    maintainers = with maintainers; [ ];
  };
}
