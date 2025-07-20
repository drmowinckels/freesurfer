ARG R_VERSION="4.4.1"

# Start with the Rocker R-version image
FROM rocker/r-ver:${R_VERSION}

# Set environment variables for FreeSurfer
ENV FREESURFER_HOME=/opt/freesurfer
# FIX TYPO HERE: FREESURSER_HOME -> FREESURFER_HOME
ENV PATH="${PATH}:${FREESURFER_HOME}/bin:${FREESURFER_HOME}/mni/bin"
ENV SUBJECTS_DIR="${FREESURFER_HOME}/subjects"
ENV MNI_DIR="${FREESURFER_HOME}/mni"

# Install system dependencies for FreeSurfer
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
    build-essential \
    libgomp1 \
    tcsh \
    libjpeg-dev \
    libtiff-dev \
    libpng-dev \
    libglu1-mesa \
    libsm6 \
    libice6 \
    libxrender1 \
    libxtst6 \
    libxi6 \
    libxmu6 \
    libxxf86vm1 \
    libxft2 \
    libfreetype6 \
    libgl1 \
    libfontconfig1 \
    libglu1-mesa-dev \
    wget \
    unzip \
    ca-certificates \
    software-properties-common && \
    rm -rf /var/lib/apt/lists/*


# === FreeSurfer Installation ===
ARG FS_VERSION="7.4.1" 
RUN mkdir -p /opt/freesurfer_downloads && \
    wget -q https://surfer.nmr.mgh.harvard.edu/pub/dist/freesurfer/${FS_VERSION}/freesurfer-linux-ubuntu22_amd64-${FS_VERSION}.tar.gz -O /opt/freesurfer_downloads/freesurfer.tar.gz && \
    tar -xzf /opt/freesurfer_downloads/freesurfer.tar.gz -C /opt && \
    mv /opt/freesurfer-linux-ubuntu22_amd64-${FS_VERSION} /opt/freesurfer && \
    rm -rf /opt/freesurfer_downloads && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN mkdir -p ${FREESURFER_HOME}
RUN touch ${FREESURFER_HOME}/.license
WORKDIR /app

RUN R -e "install.packages(c('devtools', 'pak'), repos = 'https://cloud.r-project.org')"

