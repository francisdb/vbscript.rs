#!/usr/bin/env bash
set -e

# make sure we are in the correct directory
cd "$(dirname "$0")"

repos=(
  "vpx-standalone-scripts https://github.com/jsm174/vpx-standalone-scripts.git"
  "vpinball https://github.com/vpinball/vpinball.git"
  "sverrewl-vpxtable-scripts https://github.com/sverrewl/vpxtable_scripts.git"
)

for repo in "${repos[@]}"; do
  IFS=' ' read -r -a array <<< "$repo"
  dir=${array[0]}
  url=${array[1]}

  if [ -d "$dir" ]; then
    echo "Updating $dir"
    cd "$dir"
    git pull
    cd ..
  else
    echo "Cloning $dir"
    git clone --depth 1 "$url" "$dir"
  fi
done