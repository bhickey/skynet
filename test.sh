#!/bin/bash
ghc --make Skynet
cp Skynet tools/
cd tools
./test_bot.sh Skynet
