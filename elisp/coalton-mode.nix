{ trivialBuild, fetchFromGitHub }:
trivialBuild rec {
  pname = "coalton-mode";
  version = "1.0.0";
  src = fetchFromGitHub {
    owner = "coalton-lang";
    repo = "coalton-mode";
    rev = "65787adb1711b5968207cc52442e6ce0d9a1f478";
    hash = "sha256-S0dNDrP3t4I0E/tQJeD4jz27+FfyCRCtmASfgmHulQw=";
  };
}
