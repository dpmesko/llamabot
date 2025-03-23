#!/bin/bash

set -e
stack build
cp /home/dpmes/llamabot/.stack-work/install/x86_64-linux/5a73bdcfbe4d226bc9355406b65cdb1a0987447f63e9bf65625a4797d6ab1611/9.0.2/bin/llamabot ./
cp /home/dpmes/llamabot/.stack-work/install/x86_64-linux/5a73bdcfbe4d226bc9355406b65cdb1a0987447f63e9bf65625a4797d6ab1611/9.0.2/bin/llamabot-slash-command ./
