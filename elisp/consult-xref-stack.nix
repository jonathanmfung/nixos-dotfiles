{
  trivialBuild,
  fetchFromGitHub,
  consult,
}:
trivialBuild rec {
  pname = "consult-xref-stack";
  version = "1.0.0";
  src = fetchFromGitHub {
    owner = "brett-lempereur";
    repo = "consult-xref-stack";
    rev = "aa9bbf7a3ff43353b7c10595b3d13887b213466b";
    hash = "sha256-TUACrdK/AxwFn7/HFuaHGkq0hscVtPIXy4TDf0rLYqo=";
  };
  packageRequires = [ consult ];
}
