#!/bin/bash

set -e
stack build
cp ~/llamabot/.stack-work/install/x86_64-linux-tinfo6/9b6cb93a50f935978de5337a9ac203bc719bfd9bc937bf2e822233eaa95e4614/9.0.2/bin/llamabot ./
