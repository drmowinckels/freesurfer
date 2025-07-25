# Stage 1: FreeSurfer Base
FROM ubuntu:22.04 AS freesurfer-base
ARG FS_VERSION=8.0.0

## Set a default user. Available via runtime flag `--user docker`
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly).
RUN useradd -s /bin/bash -m docker \
    && usermod -a -G staff docker

## This was not needed before but we need it now
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        wget \
        software-properties-common \
        dirmngr \
        ed \
        gpg-agent \
        less \
        vim-tiny \
        gnupg \
        curl \
        ca-certificates \
        build-essential \
        libxt-dev \
        libglu1-mesa-dev \
        libncurses6 \
        libffi8 \
        locales \
        locales-all

# Configure default locale
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen en_US.utf8 \
    && /usr/sbin/update-locale LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV TZ=UTC

# Set environment variables for FreeSurfer
ENV FREESURFER_HOME=/usr/local/freesurfer
ENV SUBJECTS_DIR=${FREESURFER_HOME}/subjects

# Download and install FreeSurfer
RUN wget -q https://surfer.nmr.mgh.harvard.edu/pub/dist/freesurfer/${FS_VERSION}/freesurfer_ubuntu22-${FS_VERSION}_amd64.deb -O freesurfer.deb \
    && apt-get install -y ./freesurfer.deb \
    && rm freesurfer.deb

CMD ["bash"]

# Stage 2: Final Image with R
FROM freesurfer-base AS freesurfer-r
ARG R_VERSION=4.3.1

# Add R-related repositories
RUN wget -q -O - https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc \
    && add-apt-repository --yes "ppa:marutter/rrutter4.0" \
    && add-apt-repository --yes "ppa:c2d4u.team/c2d4u4.0+" \
    && add-apt-repository --yes "ppa:edd/misc"

# Install R and additional packages
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        littler \
        r-cran-docopt \
        r-cran-littler \
		r-base=${R_VERSION} \
		r-base-dev=${R_VERSION} \
        r-base-core=${R_VERSION} \
		r-recommended=${R_VERSION} \
    && chown root:staff "/usr/local/lib/R/site-library" \
    && chmod g+ws "/usr/local/lib/R/site-library" \
    && ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
    && ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
    && ln -s /usr/lib/R/site-library/littler/examples/installBioc.r /usr/local/bin/installBioc.r \
    && ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
    && ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
    && ln -s /usr/lib/R/site-library/littler/examples/update.r /usr/local/bin/update.r \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
    && rm -rf /var/lib/apt/lists/*

CMD ["bash"]
