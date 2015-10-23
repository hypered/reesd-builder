# Reesd Builder

The `images.reesd.com/reesd/builder` Docker image is used to clone a GitHub
repository and `docker build` the images found in it.

## Example usage

The repository to build must contain an `Imagename` file per image to build.
An example repository is https://github.com/hypered/reesd-hello.

The builder needs:

- Access to a Docker daemon. This is done by mounting the Docker socket.

- A `.ssh` directory providing a private SSH key to `git clone` the repository
  to build. Normally the public SSH key is added as a read-only "Deploy key" to
  the repository.

- An optional `.dockercfg` file (when present, it is used to push the image).

    > docker run \
        -v /var/run/docker.sock:/var/run/docker.sock \
        -v /path/to/github-deploy-ssh:/home/worker/.ssh \
        -v /path/to/dockercfg:/home/worker/.dockercfg \
        images.reesd.com/reesd/builder \
          /home/worker/checkout-and-build.sh \
          git@github.com:hypered/reesd-hello.git \
          reesd-hello.git \
          master
