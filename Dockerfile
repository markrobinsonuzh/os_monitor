FROM rocker/shiny-verse

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    git \
    libxml2 \
    libxml2-dev

# Install R packages
RUN install2.r rcrossref 
RUN install2.r roadoi 
RUN install2.r 	mongolite 
RUN install2.r  shinyjs 
RUN install2.r 	rentrez 
RUN install2.r 	RefManageR 
RUN install2.r	scholar 
RUN install2.r	here 
RUN install2.r	rorcid 
RUN install2.r	testthat 
RUN install2.r	devtools 
RUN install2.r	shinyhelper 
RUN install2.r	UpSetR

RUN mkdir -p /srv/shiny-server/os_monitor/shiny_app
RUN mkdir -p /srv/shiny-server/os_monitor/uzhOS

COPY shiny_app /srv/shiny-server/os_monitor/shiny_app
COPY uzhOS /srv/shiny-server/os_monitor/uzhOS

COPY shiny-server.conf /etc/shiny-server

EXPOSE 3839
	
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
