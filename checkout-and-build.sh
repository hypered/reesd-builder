#! /usr/bin/env bash

# Build the Dockerfile for which there is an Imagename.

# Only build master or blue branches.
case "$3" in
  "master")
    TAG=""
    ;;
  "blue")
    TAG=":${3}"
    ;;
  *)
    exit 0
    ;;
esac

# Repository, directory, branch.
echo "Checkout of $1 $2 $3..."

/home/worker/checkout.sh $1 $2 $3
for i in `find /home/worker/checkout -name Imagename` ; do
  imagename=$(cat $i)${TAG}
  pushname=$(cat $i)${TAG:-:latest}
  imagedir=$(dirname $i)
  commit=$(git --git-dir /home/worker/gits/$2 rev-parse $3)

  echo "Building ${imagename}..."
  cat /build-info.txt \
    | sed -e "s=__REPOSITORY__=$1=" \
    | sed -e "s=__BRANCH__=$3=" \
    | sed -e "s=__COMMIT__=${commit}=" \
    | sed -e "s=__IMAGE__=${imagename}=" \
    > ${imagedir}/BUILD-INFO
  echo "ADD BUILD-INFO /" >> ${imagedir}/Dockerfile
  sudo docker build --force-rm --no-cache -t ${imagename} ${imagedir}

  if [ -f /home/worker/.dockercfg ] ; then
    echo "Pushing ${pushname}..."
    sudo docker push ${pushname}
  fi

  if [ -f /slack-hook-url.txt ] ; then
    echo "Notifying Slack for ${imagename}..."
    HOOK=$(cat /slack-hook-url.txt)
    cat /slack-payload.txt \
      | sed -e "s=__REPOSITORY__=$1=" \
      | sed -e "s=__BRANCH__=$3=" \
      | sed -e "s=__IMAGE__=${imagename}=" \
      > /tmp/payload.txt
    curl -s -X POST --data-urlencode payload@/tmp/payload.txt ${HOOK}
  fi
done
