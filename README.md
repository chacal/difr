# Intro

This repository contains a port of [Difr](https://github.com/wspringer/difr) to support git log -p output format instead of git diff.

# Building & running

    ./sbt assembly
    git log -p | ./difr -o test.html

Open test.html in browser
