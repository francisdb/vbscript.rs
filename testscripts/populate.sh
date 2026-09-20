#!/usr/bin/env bash
set -e

# make sure we are in the correct directory
cd "$(dirname "$0")"

# Each entry is "dir url commit".
# The commit is pinned so the test corpus is reproducible and CI is not broken by
# upstream changes. Bump these SHAs deliberately when refreshing the corpus.
repos=(
  "vpx-standalone-scripts https://github.com/jsm174/vpx-standalone-scripts.git 8f0b44224ace068ff2dd0d4c9a3db03cd3c925ff"
  "vpinball/scripts https://github.com/vpinball/vpinball.git f40b291411cf3d31f575db8953190322c6b9ef50"
  "sverrewl-vpxtable-scripts https://github.com/sverrewl/vpxtable_scripts.git 9e4a4a519405e14015ed009f85f34a2ecef3da38"
  "wine-vbscript/dlls/vbscript/tests https://github.com/wine-mirror/wine.git 9ce1651515d93d9760e2438a53bf2c117238bc2b"
)

for repo in "${repos[@]}"; do
  IFS=' ' read -r dir url sha <<< "$repo"

  # Split the directory into the clone target and an optional subdirectory, and
  # build a sparse-checkout pattern that selects only the .vbs files we test (the
  # `**/*.vbs` extension filter requires non-cone mode).
  repo_dir=${dir%%/*}
  if [ "$dir" = "$repo_dir" ]; then
    pattern='**/*.vbs'
  else
    pattern="${dir#*/}/**/*.vbs"
  fi

  # Skip if already checked out at the pinned commit.
  if [ "$(git -C "$repo_dir" rev-parse HEAD 2>/dev/null)" = "$sha" ]; then
    echo "$repo_dir already at $sha"
    continue
  fi

  # Sparse + `tree:0` partial + shallow so only the .vbs blobs at the pinned
  # commit are fetched, never the whole repo (e.g. all of wine).
  echo "Fetching $repo_dir at $sha"
  rm -rf "$repo_dir"
  git init -q "$repo_dir"
  # some scripts have a path of over 260 characters, which windows needs this for
  git -C "$repo_dir" config core.longpaths true
  git -C "$repo_dir" remote add origin "$url"
  git -C "$repo_dir" sparse-checkout set --no-cone "$pattern"
  git -C "$repo_dir" fetch -q --depth 1 --filter=tree:0 origin "$sha"
  git -C "$repo_dir" checkout -q FETCH_HEAD
done
