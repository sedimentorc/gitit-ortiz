#!/bin/sh
set -e
umask 002
export PATH=/home/root/.cabal/bin:$PATH

echo "enter script"
gitit -f /data/gitit.conf