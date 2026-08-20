#!/bin/bash
cd ~/airbnb-listings-
git pull
Rscript -e 'targets::tar_make()' > pipeline_log.txt 2>&1
echo $? > pipeline_status.txt