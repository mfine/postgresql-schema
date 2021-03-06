name:                  postgresql-schema
version:               VERSION
synopsis:              PostgreSQL Schema Management
description:           Please see README.md
homepage:              https://github.com/mfine/postgresql-schema
license:               BSD3
license-file:          LICENSE
author:                Mark Fine
maintainer:            mark.fine@gmail.com
copyright:             Copyright (C) 2015 Mark Fine
category:              Database
build-type:            Simple
cabal-version:         >= 1.22
data-files:            migrations/20130318-105434.sql
                     , migrations/20130318-105456.sql

library
  exposed-modules:     Database.PostgreSQL.Schema
  default-language:    Haskell2010
  hs-source-dirs:      src
  ghc-options:         -Wall -fno-warn-orphans
  build-depends:       base >= 4.8 && < 5
                     , basic-prelude
                     , postgresql-simple
                     , shelly
                     , text
  default-extensions:  NoImplicitPrelude
                       OverloadedStrings

executable schema-add
  hs-source-dirs:      main
  main-is:             Add.hs
  ghc-options:         -Wall
  default-language:    Haskell2010
  build-depends:       base >= 4.8 && < 5
                     , basic-prelude
                     , optparse-applicative
                     , postgresql-schema
                     , shelly
                     , text
                     , time
  default-extensions:  NoImplicitPrelude
                       OverloadedStrings
                       RecordWildCards

executable schema-apply
  hs-source-dirs:      main
  main-is:             Apply.hs
  other-modules:       Paths_postgresql_schema
  ghc-options:         -Wall
  default-language:    Haskell2010
  build-depends:       base >= 4.8 && < 5
                     , basic-prelude
                     , optparse-applicative
                     , postgresql-schema
                     , shelly
                     , text
  default-extensions:  NoImplicitPrelude
                       OverloadedStrings
                       RecordWildCards

executable schema-clear
  hs-source-dirs:      main
  main-is:             Clear.hs
  ghc-options:         -Wall
  default-language:    Haskell2010
  build-depends:       base >= 4.8 && < 5
                     , basic-prelude
                     , optparse-applicative
                     , postgresql-schema
                     , shelly
                     , text
  default-extensions:  NoImplicitPrelude
                       OverloadedStrings
                       RecordWildCards

source-repository head
  type:     git
  location: https://github.com/mfine/postgresql-schema
