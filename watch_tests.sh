#!/bin/bash
# Continuous test runner using entr
# Install entr first: brew install entr
# Usage: ./watch_tests.sh

find src test -name "*.erl" | entr -c rebar3 eunit
