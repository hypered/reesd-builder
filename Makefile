all: .builder_touched

STACK_IMAGE ?= 7.8.4

.builder_touched: Dockerfile dist/build/reesd-build/reesd-build
	docker build -t images.reesd.com/reesd/builder .
	touch .builder_touched

reesd-builder-docker-image.tar.xz: .builder_touched
	rm -f reesd-builder-docker-image.tar.xz
	docker save images.reesd.com/reesd/builder > reesd-builder-docker-image.tar
	xz reesd-builder-docker-image.tar

dist/build/reesd-build/reesd-build: bin/reesd-build.hs reesd-builder.cabal Reesd/Commands/Build.hs
	docker run \
          -v `pwd`:/home/gusdev/reesd-builder \
          images.reesd.com/reesd/stack:$(STACK_IMAGE) \
          cabal install reesd-builder/reesd-builder.cabal
