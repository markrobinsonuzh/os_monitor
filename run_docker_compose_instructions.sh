#!/bin/bash


# build containers, set up network, etc.
docker-compose build

# run containers and detach (-d)
docker-compose up -d
