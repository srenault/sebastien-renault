#!/bin/bash

ghc --make site.hs
./site clean
./site build
cp -r _site/* ../srenault.github.io
