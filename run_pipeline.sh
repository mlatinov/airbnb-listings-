#!/bin/bash
cd ~/airbnb-listings-
git pull
Rscript -e 'targets::tar_make()'