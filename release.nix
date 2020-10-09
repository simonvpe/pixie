{ project ? import ./nix {} }:

{
  inherit (project) ci;
}
