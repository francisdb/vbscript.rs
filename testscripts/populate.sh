#!/usr/bin/env bash
set -e

# make sure we are in the correct directory
cd "$(dirname "$0")"

repos=(
  "vpx-standalone-scripts https://github.com/jsm174/vpx-standalone-scripts.git"
  "vpinball/scripts https://github.com/vpinball/vpinball.git"
  "sverrewl-vpxtable-scripts https://github.com/sverrewl/vpxtable_scripts.git"
)

for repo in "${repos[@]}"; do
  IFS=' ' read -r -a array <<< "$repo"
  dir=${array[0]}
  url=${array[1]}

  # Split the directory into repo and subdirectory
  IFS='/' read -r -a dir_parts <<< "$dir"
  repo_dir=${dir_parts[0]}
  sub_dir=${dir_parts[1]}

  # If a subdirectory is provided, perform a sparse checkout of subdirectory
  if [ -n "$sub_dir" ]; then
    rm -rf "$repo_dir"
    git clone -n --depth=1 --filter=tree:0 "$url" "$repo_dir"
    cd "$repo_dir"
    git sparse-checkout set --no-cone "$sub_dir"
    git checkout
    cd ..
  else
    if [ -d "$dir" ]; then
      echo "Updating $dir"
      cd "$dir"
      git fetch origin --depth 1
      git reset --hard origin/HEAD
      cd ..
    else
      echo "Cloning $dir"
      git clone --depth 1 "$url" "$dir"
    fi
  fi
done