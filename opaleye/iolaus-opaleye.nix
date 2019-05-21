{ mkDerivation, aeson, base, dhall, ekg-core, filepath, lens, mtl
, opaleye, postgresql-simple, postgresql-simple-migration
, product-profunctors, resource-pool, retry, stdenv, text
}:
mkDerivation {
  pname = "iolaus-opaleye";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base dhall ekg-core filepath lens mtl opaleye
    postgresql-simple postgresql-simple-migration product-profunctors
    resource-pool retry text
  ];
  executableHaskellDepends = [
    aeson base dhall ekg-core filepath lens mtl opaleye
    postgresql-simple postgresql-simple-migration product-profunctors
    resource-pool retry text
  ];
  description = "Common bits of Opaleye that you need in all your apps";
  license = stdenv.lib.licenses.bsd2;
}
