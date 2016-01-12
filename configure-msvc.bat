@echo off
mkdir build-32bit & pushd build-32bit
cmake -G "Visual Studio 14 2015" ..
popd
mkdir build-64bit & pushd build-64bit
cmake -G "Visual Studio 14 2015 Win64" ..
popd
