#!/bin/bash

cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint.hs
cabal v2-run generator-script -v0 -- -o src/Data/Tuple/Constraint/ClassNewtype.hs --class-newtype
