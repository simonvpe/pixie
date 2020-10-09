{ project ? import ./nix {} }:

(
  project.pixie.overrideAttrs (
    pixie: {
      nativeBuildInputs = (pixie.nativeBuildInputs or []) ++ project.devTools;
      shellHook = (pixie.shellHook or "") + ''
        ${project.ci.pre-commit-check.shellHook}
      '';
    }
  )
)
