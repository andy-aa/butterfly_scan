FROM rocker/rstudio

EXPOSE 8787

USER root

RUN apt-get update
# RUN apt-get install -y apt-utils 
# RUN apt-get install -y dialog apt-utils
RUN apt-get install -y mc vim
RUN apt-get install -y texlive-full
# RUN apt-get install -y libx11-dev
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y -q libx11-dev xserver-xorg-dev xorg-dev
# libx11-dev
# xorg
#  openbox

# libxml2
RUN apt-get install -y r-base \
    && Rscript -e 'install.packages("imager", repos="https://cloud.r-project.org")'

# USER rstudio

# RUN apt-get install -y r-base \
#     && Rscript -e 'install.packages("imager", repos="https://cloud.r-project.org")'
# RUN apt-get install libx11-dev
