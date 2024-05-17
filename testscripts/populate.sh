#!/usr/bin/env bash
set -e

# make sure we are in the correct directory
cd "$(dirname "$0")"

rm -rf vpx-standalone-scripts
git clone --depth 1 https://github.com/jsm174/vpx-standalone-scripts.git

rm -rf vpinball
git clone --depth 1 https://github.com/vpinball/vpinball.git

rm -rf vpxtable-scripts
git clone --depth 1 https://github.com/sverrewl/vpxtable_scripts.git
