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
RUN install2.r \
	rcrossref \
	roadoi \
	mongolite \
	shinyjs \
	rentrez \
	RefManageR \
	scholar \
	here \
	rorcid \
	testthat \
	devtools \
	shinyhelper

RUN mkdir -p /srv/shiny-server/os_monitor/shiny_app
RUN mkdir -p /srv/shiny-server/os_monitor/uzhOS

COPY shiny_app /srv/shiny-server/os_monitor/shiny_app
COPY uzhOS /srv/shiny-server/os_monitor/uzhOS

COPY shiny-server.conf /etc/shiny-server

EXPOSE 3839
	
# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
