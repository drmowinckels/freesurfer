
ARG R_VERSION=4.5.0

FROM rocker/r-ver:${R_VERSION}
ARG FS_VERSION=8.0.0

# Install necessary dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    wget \
    gnupg \
    ca-certificates \
    build-essential \
    libxt-dev \
    libglu1-mesa-dev \
    curl \
    locales \
    locales-all

# Set environment variables for FreeSurfer
ENV FREESURFER_HOME=/usr/local/freesurfer
ENV SUBJECTS_DIR=${FREESURFER_HOME}/subjects

# Download and install FreeSurfer using the deb package
RUN wget -q https://surfer.nmr.mgh.harvard.edu/pub/dist/freesurfer/${FS_VERSION}/freesurfer_ubuntu20-${FS_VERSION}_amd64.deb \
    -O freesurfer.deb && \
    apt-get install -y ./freesurfer.deb && \
    rm freesurfer.deb

# Verify installations
RUN freeview --version

CMD ["R"]
