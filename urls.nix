with builtins;

let

  pkgs = import ~/src/nixpkgs {};
  lib = import <nixpkgs/lib>;

  isDrvSafe = x: (tryEval x).success && isAttrs x && x ? type && (tryEval x.type).success && x.type == "derivation";
  getSrc = p: if (tryEval p).success && p ? src && (tryEval p.src).success && p.src ? urls then p.src.urls else [];
  getPos = p: if (tryEval p).success && p ? meta && (tryEval p.meta).success && p.meta ? position && p.meta.position != null then p.meta.position else null;
  getPosDes = input: let
    x = lib.reverseList (lib.splitString ":" input);
  in {
    line = lib.toInt (lib.head x);
    file = lib.concatStringsSep ":" (lib.reverseList (lib.tail x));
  };
in 
map (val: val // { path = getPosDes val.path; }) (
filter (val: val.urls != [] && val.path != null) (
map (name: { inherit name; path = getPos pkgs.${name}; urls = lib.flatten (getSrc pkgs.${name}); }) (
filter (name: isDrvSafe pkgs.${name}) (builtins.attrNames pkgs))))
