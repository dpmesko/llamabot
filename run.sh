#!/bin/bash

set -e
stack build
cp /home/daniel/llamabot/.stack-work/install/x86_64-linux-tinfo6/8061042c083b21d50d3308ab89d7a25884f2983fea1740bfd66f3ef7fbcccad1/9.0.2/bin/llamabot ./
./llamabot

