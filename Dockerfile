FROM rocker/r-ver:4.1.0
RUN apt-get update && apt-get install -y  libcurl4-openssl-dev libglpk-dev libgmp-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("magrittr",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("withr",upgrade="never", version = "2.4.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.9")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.33")'
RUN Rscript -e 'remotes::install_version("tidyr",upgrade="never", version = "1.1.3")'
RUN Rscript -e 'remotes::install_version("RColorBrewer",upgrade="never", version = "1.1-2")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("covr",upgrade="never", version = "3.5.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.2")'
RUN Rscript -e 'remotes::install_version("igraph",upgrade="never", version = "1.2.6")'
RUN Rscript -e 'remotes::install_version("reshape2",upgrade="never", version = "1.4.4")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
RUN Rscript -e 'remotes::install_version("openxlsx",upgrade="never", version = "4.2.3")'
RUN Rscript -e 'remotes::install_version("formattable",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("rintrojs",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("shinythemes",upgrade="never", version = "1.2.0")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');OpenRepGrid.ic::ic()"
