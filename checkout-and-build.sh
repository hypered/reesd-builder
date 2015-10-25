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

echo "Checkout of $1 $2 $3..."

/home/worker/checkout.sh $1 $2 $3
for i in `find /home/worker/checkout -name Imagename` ; do
  imagename=$(cat $i)${TAG}
  pushname=$(cat $i)${TAG:-:latest}
  echo "Building ${imagename}..."
  sudo docker build --force-rm --no-cache -t ${imagename} $(dirname $i)
  if [ -f /home/worker/.dockercfg ] ; then
    echo "Pushing ${pushname}..."
    sudo docker push ${pushname}
  fi
  if [ -f /slack-hook-url.txt ] ; then
    HOOK=$(cat /slack-hook-url.txt)
    cat /slack-payload.txt \
      | sed -e "s=__REPOSITORY__=$1=" \
      | sed -e "s=__BRANCH__=$3=" \
      | sed -e "s=__IMAGE__=${imagename}=" \
      > /tmp/payload.txt
    echo "Notifying Slack for ${imagename}..."
    curl -s -X POST --data-urlencode payload@/tmp/payload.txt ${HOOK}
  fi
done
