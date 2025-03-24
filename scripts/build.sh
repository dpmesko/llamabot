#!/bin/bash


EXE_DIR="./executables"


stack build

if [ ! -d "$EXE_DIR" ]; then
  mkdir "$EXE_DIR" 
fi


# TODO: are these directory paths static?
sudo cp ~/llamabot/.stack-work/install/x86_64-linux/5a73bdcfbe4d226bc9355406b65cdb1a0987447f63e9bf65625a4797d6ab1611/9.0.2/bin/llamabot ./"$EXE_DIR"
sudo cp ~/llamabot/.stack-work/install/x86_64-linux/5a73bdcfbe4d226bc9355406b65cdb1a0987447f63e9bf65625a4797d6ab1611/9.0.2/bin/llamabot-slash-command ./"$EXE_DIR"
