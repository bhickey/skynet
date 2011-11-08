#!/bin/bash
ghc --make -Wall -Werror Skynet || exit 1
cp Skynet tools/
cd tools
./test_bot.sh Skynet
