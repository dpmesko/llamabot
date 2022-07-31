#!/bin/bash

set -e
stack build
cp /home/daniel/llamabot/.stack-work/install/x86_64-linux-tinfo6/b579410872a6dc796a18eed72257ad3f4f3f459335eafbb6a8d74cfcd0fad81c/9.0.2/bin/llamabot ./

./llamabot

