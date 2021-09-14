{ lib
, buildPythonPackage
, fetchFromGitHub
, numpy
, scipy
, requests
, tornado
, pyzmq
, six
, jsonpatch
, websocket-client
, torchfile
}:

buildPythonPackage rec {
  pname = "visdom";
  version = "0.1.8.9";

  src = fetchFromGitHub {
    owner = "fossasia";
    repo = pname;
    rev = "v${version}";
    sha256 = "0fraasicmj32m27ay08ggfjdq1syjy3psf9b37wbkmnjw8645sly";
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
  ];

  doCheck = false;

  pythonImportsCheck = [ "visdom" ];

  meta = with lib; {
    description = "A flexible tool for creating, organizing, and sharing visualizations of live, rich data. Supports Torch and Numpy.";
    homepage = "https://github.com/fossasia/visdom";
    license = licenses.asl20;
    maintainers = with maintainers; [ ];
  };
}
