{ lib
, buildPythonPackage
, fetchFromGitHub
,  gym
,  matplotlib
,  nes-py
,  numpy
,  pyopengl
,  pygame
,  pyglet
,  setuptools
,  tqdm
,  twine
}:

buildPythonPackage rec {
  pname = "gym-super-mario-bros";
  version = "7.3.2";

  src = fetchFromGitHub {
    owner = "Kautenja";
    repo = pname;
    rev = version;
    sha256 = "1p99zllivj1457vkpwpkg1azij4g665qxp7aggvf0lriww6mmxcd";
  };

  propagatedBuildInputs = [
    gym
    matplotlib
    nes-py
    numpy
    pyopengl
    pygame
    pyglet
    setuptools
    tqdm
    twine
  ];

  # The test needs MuJoCo that is not free library.
  doCheck = false;

  pythonImportsCheck = [ "gym_super_mario_bros" ];

  meta = with lib; {
    description = "An OpenAI Gym interface to Super Mario Bros. & Super Mario Bros. 2 (Lost Levels) on The NES";
    homepage = "https://github.com/Kautenja/gym-super-mario-bros";
    license = licenses.free;
    maintainers = with maintainers; [ ];
  };
}
