#!/bin/bash

# Prerequisites
brew install llvm

# Rtags repo
git clone https://github.com/Andersbakken/rtags.git
cd rtags
git submodule init
git submodule update
mkdir build
cd build
cmake ../ -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1
make
