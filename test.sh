#!/bin/bash
ghc --make MyBot
cp MyBot tools/
cd tools
./test_bot.sh MyBot
