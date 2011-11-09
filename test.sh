#!/bin/bash
ghc --make -O3 -Wall -Werror Skynet || exit 1
cp Skynet tools/
cd tools
./test_bot.sh Skynet
