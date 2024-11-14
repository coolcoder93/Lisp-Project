#!/bin/sh

SBCL="sbcl"

# For debug builds add:
# --eval "(declaim (optimize (speed 0) (space 0) (debug 3) (safety 3)))" \
# For release builds add:
# --eval "(declaim (optimize (speed 2) (space 1) (debug 0) (safety 1)))" \
set -x
$SBCL --noinform \
      --load "build.lisp"
