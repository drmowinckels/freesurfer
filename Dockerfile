# Stage 1: FreeSurfer installation
FROM debian:bullseye-slim AS freesurfer-base
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
    language-pack-en

# Set environment variables for FreeSurfer
ENV FREESURFER_HOME=/usr/local/freesurfer
ENV SUBJECTS_DIR=/usr/local/freesurfer/subjects
ENV FS_LICENSE=/usr/local/freesurfer/license.txt

# Download and install FreeSurfer using the deb package
RUN wget -q https://surfer.nmr.mgh.harvard.edu/pub/dist/freesurfer/${FS_VERSION}/freesurfer_ubuntu20-${FS_VERSION}_amd64.deb \
    -O freesurfer.deb && \
    apt-get install -y ./freesurfer.deb && \
    rm freesurfer.deb

# Stage 2: Main image for specific R version
FROM freesurfer-base AS r-freesurfer
ARG R_VERSION=4.2.0

# Install R
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3F32EE77E331692F && \
    echo "deb http://cran.r-project.org/bin/linux/debian bullseye-cran40/" > /etc/apt/sources.list.d/cran.list && \
    apt-get update && \
    apt-get install -y --no-install-recommends r-base=${R_VERSION}* && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Verify installations
RUN R --version && \
    freeview --version

# Set working directory
WORKDIR /data

# Expose necessary ports and set the default command
EXPOSE 8080
CMD ["R"]
