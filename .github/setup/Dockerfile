
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
    locales-all \
    software-properties-common && \
    apt-get clean

# Install older versions of dependencies for FreeSurfer
RUN echo "deb http://archive.ubuntu.com/ubuntu focal main universe" > /etc/apt/sources.list.d/focal.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends \
    libncurses5 \
    libffi7 \
    libc6=2.31-0ubuntu9.9 && \
    apt-get clean

# Set environment variables for FreeSurfer
ENV FREESURFER_HOME=/usr/local/freesurfer
ENV SUBJECTS_DIR=${FREESURFER_HOME}/subjects

# Download Freesurfer deb package
RUN wget -q https://surfer.nmr.mgh.harvard.edu/pub/dist/freesurfer/${FS_VERSION}/freesurfer_ubuntu20-${FS_VERSION}_amd64.deb -O freesurfer.deb 

# Install FreeSurfer using the deb package
RUN apt-get install -y ./freesurfer.deb && \
    rm freesurfer.deb

# Verify installations
RUN freeview --version

CMD ["R"]
