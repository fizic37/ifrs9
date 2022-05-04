FROM rocker/r-ver:4.1.0
RUN apt-get update && apt-get install -y  libicu-dev libpng-dev python && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("parsnip",upgrade="never", version = "0.1.7")'
RUN Rscript -e 'remotes::install_version("rsample",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("recipes",upgrade="never", version = "0.2.0")'
RUN Rscript -e 'remotes::install_version("workflows",upgrade="never", version = "0.2.4")'
RUN Rscript -e 'remotes::install_version("readxl",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("readr",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("glmnet",upgrade="never", version = "4.1-2")'
RUN Rscript -e 'remotes::install_version("embed",upgrade="never", version = "0.1.5")'
RUN Rscript -e 'remotes::install_version("broom",upgrade="never", version = "0.7.10")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
#RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone