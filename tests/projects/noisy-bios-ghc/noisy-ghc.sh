#!/usr/bin/env bash
# Simulates the noise produced by tools like darcs, which output warnings
# (e.g. "WARNING: creating a nested repository.") before the actual command
# output when invoked inside a managed repository.
echo "WARNING: creating a nested repository."
exec ghc "$@"
