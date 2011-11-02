#!/bin/bash
ghc --make Skynet || exit 1
cp Skynet tools/
cd tools
./test_bot.sh Skynet
