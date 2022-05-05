@ECHO OFF
SET rootDir=%~dp0..
make -C %rootDir% build
%rootDir%\Marksman\bin\Debug\net6.0\marksman.exe