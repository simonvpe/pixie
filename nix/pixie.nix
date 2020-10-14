{ mkDerivation
, base
, containers
, mtl
, parsec
, pretty
, repline
, stdenv
, text
, transformers
}:
mkDerivation {
  pname = "pixie";
  version = "0.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    mtl
    parsec
    pretty
    repline
    text
    transformers
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
