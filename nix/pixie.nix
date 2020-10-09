{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "pixie";
  version = "0.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
