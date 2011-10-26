#!/bin/bash
echo "FIXME! FIXME! FIXME!"
ghc --make MyBot
mv MyBot tools/
tools/test_bot.sh MyBot
