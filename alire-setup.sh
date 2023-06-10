#!/bin/sh
./configure --enable-alire
test -f adafr.properties || cp adafr-sqlite.properties adafr.properties
make setup
