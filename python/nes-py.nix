{ lib
, buildPythonPackage
, fetchFromGitHub
,  gym
,  numpy
,  pyglet
,  tqdm
,  twine
}:

buildPythonPackage rec {
  pname = "nes-py";
  version = "8.1.7";

  src = fetchFromGitHub {
    owner = "Kautenja";
    repo = pname;
    rev = version;
    sha256 = "0lx3lq13p2q5vz522yx8775sva73kvdqhb1zcplqxwffdg7k3bw4";
  };

  propagatedBuildInputs = [
    gym
    numpy
    pyglet
    tqdm
    twine
  ];

  # The test needs MuJoCo that is not free library.
  doCheck = false;

  # does not works
  strictDepts = false;

  # does not works either
  # patchPhase = ''
  #   substituteInPlace requirements.txt \
  #    --replace "<=1.5.11," ""
  # '';
  pythonImportsCheck = [ "nes_py" ];

  meta = with lib; {
    description = "A Python3 NES emulator and OpenAI Gym interface";
    homepage = "https://github.com/Kautenja/nes-py";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
