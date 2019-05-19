{ mkDerivation, aeson, base, dhall, lens, mtl, opaleye
, postgresql-simple, postgresql-simple-migration
, product-profunctors, resource-pool, retry, stdenv, text
}:
mkDerivation {
  pname = "iolaus-opaleye";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base dhall lens mtl opaleye postgresql-simple
    postgresql-simple-migration product-profunctors resource-pool retry
    text
  ];
  description = "Common bits of Opaleye that you need in all your apps";
  license = stdenv.lib.licenses.bsd2;
}
