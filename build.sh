#!/bin/bash

if [ -d ./build ]; then
    echo "Removing old build-folder"
    rm -rf "./build/"
else
    echo "Creating build directory"
fi

cp -r ./public ./build
elm make ./src/Main.elm --output ./build/main.js --optimize