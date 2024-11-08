# Use the rocker/rstudio base image
FROM rocker/rstudio:latest

# Set environment variables
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies for R and JAGS
RUN apt-get update && apt-get install -y sudo pandoc pandoc-citeproc libcurl4-gnutls-dev libcairo2-dev libxt-dev libssl-dev libxml2-dev libgit2-dev libglu1-mesa-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libboost-all-dev cmake gfortran screen jags make build-essential g++ f2c libblas-dev liblapack-dev libpcre3-dev libreadline-dev libgsl-dev autoconf libmagick++-dev tcl8.6 tk8.6 libtool* && apt-get clean

# Install RJags and other necessary R packages
RUN R -e "install.packages('r2jags', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('devtools', 'rmarkdown', 'knitr', 'tidyverse'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('dplyr', 'psych', 'ggplot2', 'patchwork', 'rstudioapi', 'naniar', 'summarytools'), repos='https://cloud.r-project.org/')"

# Expose the port for RStudio Server
EXPOSE 8787

# Add a user for RStudio
ARG USERNAME
ARG PASSWORD
RUN useradd -m -d /home/${USERNAME} -G sudo -s /bin/bash ${USERNAME} && echo "${USERNAME}:${PASSWORD}" | chpasswd

# Add the project directory
RUN mkdir -p /home/${USERNAME}/project

# Set permissions for /home/${USERNAME}
RUN chown -R ${USERNAME}:${USERNAME} /home/${USERNAME}

# Copy .Rprofile to root
COPY .Rprofile /home/${USERNAME}/.Rprofile

# Set the default command to launch RStudio Server
CMD ["/init"]

# Add the user to the sudoers file
RUN echo '${USERNAME} ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
