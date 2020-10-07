#! /bin/bash

cd /srv/shiny-server
# Downloading, preprocessing and inserting into local DB of ZORA
Rscript analysis/zora_download.R > data/download_zora.log 2>&1 && Rscript analysis/zora_preprocessing_sql.R > data/preprocess_zora.log 2>&1


