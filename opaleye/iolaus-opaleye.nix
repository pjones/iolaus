{ mkDerivation, aeson, base, dhall, ekg-core, exceptions, filepath
, lens, mtl, opaleye, postgresql-simple
, postgresql-simple-migration, product-profunctors, resource-pool
, retry, stdenv, text, transformers
}:
mkDerivation {
  pname = "iolaus-opaleye";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base dhall ekg-core exceptions filepath lens mtl opaleye
    postgresql-simple postgresql-simple-migration product-profunctors
    resource-pool retry text transformers
  ];
  executableHaskellDepends = [
    aeson base dhall ekg-core exceptions filepath lens mtl opaleye
    postgresql-simple postgresql-simple-migration product-profunctors
    resource-pool retry text transformers
  ];
  description = "Common bits of Opaleye that you need in all your apps";
  license = stdenv.lib.licenses.bsd2;
}
