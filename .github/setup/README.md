
# Docker Freesurfer R

This folder contains a Dockerfile to build a Docker image that includes both Freesurfer and R.
This image is useful for neuroimaging workflows that require both Freesurfer for structural MRI processing and R for statistical analysis or visualization.

The Docker image is built in a [github action](../workflows/build-fs-r-image.yml) and can be used to ensure a consistent testing of the functionality of the `freesurfer` R package.
The Dockerfile is designed to be flexible, allowing you to specify different versions of R and Freesurfer at build time using build arguments.

## Local use

This Dockerfile can be used locally to create a Docker image that includes both Freesurfer and R.

### Prerequisites
- Docker installed on your machine.
- Docker Buildx enabled (for multi-platform builds).


### Usage

To build the Docker image, you can use the following command:

```sh
rver=4.0.0
fsver=8.0.0

docker buildx build \
  --platform linux/amd64 \
  --file .github/setup/Dockerfile \
  --build-arg FS_VERSION=${fsver} \
  --build-arg R_VERSION=${rver} \
  -t r-freesurfer:r_${rver}-fs_${fsver} .
```

To run the Docker container, you can use the following command:

```sh
docker run --rm -it \
  -v /path/to/your/data:/data \
  r-freesurfer:r${rver}-fs_${fsver} \
  bash
```