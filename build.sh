#!/bin/bash

CARMA_DIR=`pwd`
DEPS_DIR=$CARMA_DIR/deps

REPOS=("https://github.com/dzhus/snaplet-redis.git"
       "https://github.com/f-me/snaplet-redson.git"
       "https://github.com/f-me/avaya-aes.git"
       "https://github.com/f-me/xlsx-parser.git")


if [[ ! -d "$DEPS_DIR" ]]; then
  mkdir "$DEPS_DIR"
fi

cd "$DEPS_DIR"
repos_to_rebuild=()

for repo in "${REPOS[@]}" ; do
  rPath=${repo##*/}
  rPath="$DEPS_DIR/${rPath%*.git}"
  if [[ ! -d "$rPath" ]]; then
    echo "Cloning dependent repo: $repo to $rPath"
    git clone $repo
    repos_to_rebuild=("${repos_to_rebuild[@]}" "$rPath")
  fi
done


function CheckRepo {
  repo_path=$1
  cd $repo_path
  echo "Checking $repo_path"

  git status -uno | grep "modified" > /dev/null
  if [[ $? -eq o ]]; then
    echo "    .. modified locally"
    repos_to_rebuild=("${repos_to_rebuild[@]}" "`pwd`")
    return
  fi

  git remote -v update 2>&1 | grep "up to date.*master" > /dev/null
  if [[ $? -eq 0 ]]; then
    echo "    .. up-to date"
    return
  fi

  git status -uno | grep "can be fast-forwarded" > /dev/null
  if [[ $? -eq 0 ]]; then
    echo "    .. pulling changes from remote"
    if git pull; then
      echo "    .. successfully updated"
      repos_to_rebuild=("${repos_to_rebuild[@]}" "`pwd`")
      return
    fi
  fi

  echo "    .. something went wrong"
  exit 1
}




for repo in "${REPOS[@]}"; do
  rPath=${repo##*/}
  rPath="$DEPS_DIR/${rPath%*.git}"
  CheckRepo $rPath
done


cd "$CARMA_DIR/srv"
for repo in "${repos_to_rebuild[@]}"; do
  cabal-dev add-source $repo
done

for repo in "${repos_to_rebuild[@]}"; do
  package_name=${repo##*/}
  cabal-dev install --reinstall $package_name
  if [[ ! $? -eq 0 ]]; then
    exit 1
  fi
done

cabal-dev install-deps && cabal-dev configure && cabal-dev build
#cabal-dev install

cd $CARMA_DIR
