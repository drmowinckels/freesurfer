
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
R_VERSION=3.5.0
FS_VERSION=8.0.0

docker buildx build \
  --platform linux/amd64 \
  --file .github/setup/Dockerfile \
  --build-arg FS_VERSION=${FS_VERSION} \
  --build-arg R_VERSION=${R_VERSION} \
  -t r-freesurfer:r_${R_VERSION}-fs_${FS_VERSION} .
```

To run the Docker container, you can use the following command:

```sh
docker run --rm -it \
  -v /path/to/your/data:/data \
  r-freesurfer:r${R_VERSION}-fs_${FS_VERSION} \
  bash
```