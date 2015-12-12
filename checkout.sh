#! /bin/bash

# Clone a repository and create a working copy. The clone is done in
# /home/worker/gits, so that directory can be a volume shared with a host if
# the clone must be retained. The checkout is done in /home/worker/checkout

set -e

cd gits
if [ -d "/home/worker/gits/$2" ] ; then
  git --git-dir /home/worker/gits/$2 fetch -q --tags
else
  git clone --mirror -q $1 /home/worker/gits/$2
fi
git --git-dir /home/worker/gits/$2 --work-tree /home/worker/checkout checkout $3 -- .
