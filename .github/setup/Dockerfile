
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
    locales-all && \
    apt-get clean

# Install dependencies based on Ubuntu version
RUN head -n 1 /etc/os-release && \
    if grep -q 'Ubuntu 20' /etc/os-release; then \
        apt-get install -y --no-install-recommends \
        libncurses5 \
        libffi7 && \
        apt-get clean; \
    elif grep -q 'Ubuntu 22' /etc/os-release || grep -q 'Ubuntu 24' /etc/os-release; then \
        apt-get install -y --no-install-recommends \
        libncurses6 \
        libffi8 && \
        apt-get clean; \
    else \
        echo "Unsupported OS version" && exit 1; \
    fi


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
