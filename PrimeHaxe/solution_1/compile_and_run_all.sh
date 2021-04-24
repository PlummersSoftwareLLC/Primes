#!/bin/bash

haxelib install hxcpp


haxe cpp.hxml
./bin/cpp/Main
haxe interp.hxml
haxe python.hxml
python3 bin/py.py