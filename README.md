# Reesd Builder

This repository contains a small program and associated Docker image to clone
GitHub and Bitbucket repositories, and `docker build` the images found in them.

The image is available as `images.reesd.com/reesd/builder`.


## Example usage

The following call to `docker run` represents the typical usage of this image
and is explained below.

```
> docker run \
    -v /var/run/docker.sock:/var/run/docker.sock \
    -v /path/to/github-deploy-ssh:/home/worker/ssh-keys:ro \
    -v /path/to/dockercfg:/home/worker/.dockercfg:ro \
    -e "SLACK_HOOK_URL=<see above>" \
    images.reesd.com/reesd/builder \
    reesd-build build \
    --repo git@github.com:hypered/reesd-hello \
    --image images.reesd.com/reesd/hello \
    --clone \
    --channel "#reesd"
```

This example clones the repository https://github.com/hypered/reesd-hello and
build its Dockerfile as the `images.reesd.com/reesd/hello` Docker image.

The builder needs:

- Access to a Docker daemon. This is done by mounting the Docker socket.

- A `.ssh` directory providing a private SSH key to `git clone` the repository
  to build. Normally the public SSH key is added as a read-only "Deploy key" to
  the repository.

- An optional `.dockercfg` file (when present, it is used to push the image).

- An optional `SLACK_HOOK_URL` environment variable. Its content is a "Incoming
  WebHooks" in Slack parlance and looks like

```
https://hooks.slack.com/services/xxxxxxxxx/xxxxxxxxx/xxxxxxxxxxxxxxxxxxxxxxxx
```

  When using a `SLACK_HOOK_URL`, a `--channel` option can be given.

- An optional path to the Dockerfile to build, if it is not found at
  `./Dockerfile`.


## Multi repositories

It can be useful to be able to clone multiple repositories before building a
single image. The script `build` supports multiple repositories.

The "main" repository, the one provided through the `--repo` option, is cloned
first. The other repositories, given using the `--graft` option, are checked
out within the main one.

```
docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-builder/ssh-keys:/home/worker/ssh-keys:ro \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  images.reesd.com/reesd/builder \
  reesd-build build \
  --repo git@github.com:noteed/reesd-multi-hello \
  --graft git@github.com:noteed/reesd-hello-1 \
  --graft git@github.com:noteed/reesd-hello-2 \
  --image images.reesd.com/reesd/multi-hello \
  --clone
```

If cloning is not desired, e.g. you provide the repositories through bind
mounting, you can leave out the `--clone` flag:

```
docker run \
  -v /var/run/docker.sock:/var/run/docker.sock \
  -v /home/thu/projects/reesd-multi-hello/.git:/home/worker/gits/reesd-multi-hello.git \
  -v /home/thu/projects/reesd-hello-1/.git:/home/worker/gits/reesd-hello-1.git \
  -v /home/thu/projects/reesd-hello-2/.git:/home/worker/gits/reesd-hello-2.git \
  -v /home/thu/.dockercfg:/home/worker/.dockercfg:ro \
  images.reesd.com/reesd/builder \
  reesd-build build \
  --repo git@github.com:noteed/reesd-multi-hello \
  --graft git@github.com:noteed/reesd-hello-1 \
  --graft git@github.com:noteed/reesd-hello-2 \
  --image images.reesd.com/reesd/multi-hello
```

The script creates a directory hierarchy that looks like

```
- reesd-multi-repo/
  - Dockerfile
  - reesd-repo-1/
  - reesd-repo-2/
```

Note: if you don't need anything meaningful, an empty repository such as
https://github.com/hypered/empty can be used as the "main" one.
