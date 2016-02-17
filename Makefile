all: .builder_touched

.builder_touched: Dockerfile checkout-and-build.sh dist/build/reesd-build/reesd-build
	docker build -t images.reesd.com/reesd/builder .
	touch .builder_touched

reesd-builder-docker-image.tar.xz: .builder_touched
	rm -f reesd-builder-docker-image.tar.xz
	docker save images.reesd.com/reesd/builder > reesd-builder-docker-image.tar
	xz reesd-builder-docker-image.tar

dist/build/reesd-build/reesd-build: bin/reesd-build.hs Reesd/Commands/Build.hs
	./build.sh
