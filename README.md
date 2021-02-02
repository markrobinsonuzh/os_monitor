# Open science monitor R shiny app

## Introduction
Managing publication records on Platforms such as ORCID, Google scholar and Publons can be tedious as it often requires manual updating of individual publications. This R shiny application enables the interactive exploration of the publication profiles of an author from ORCID, Google scholar, Publons and Pubmed, automatic creation of a bibtex citation file for simple uploading and updating of the respective profile.

A live version can be found here: 


### Structure
The application is organized as a R package in the folder [uzhOS](uzhOS). 
The [Dockerfile_shiny](Dockerfile_shiny) is the Docker build receipt for the shiny app.
The container build with the file [Dockerfile_rstudio](Dockerfile_rstudio) allows for development with Rstudio. 
The whole stack of containers is run using Docker compose using the file [docker-compose_shiny.yml](docker-compose_shiny.yml) or [docker-compose_reverse_proxy.yml](docker-compose_reverse_proxy.yml) (using [nginx-proxy](https://github.com/nginx-proxy/nginx-proxy)). 
