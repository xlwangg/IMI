#!/bin/bash

source /n/home00/xlwang/xlwang/bashrc/gcclassic.rocky+gnu12.minimal.env

#sbatch --mem 16000 -c 8 -t 3-12:00 -p huce_cascade run_imi.sh config.yml
sbatch --mem 16000 -c 8 -t 3-00:00 -p sapphire,seas_compute run_imi.sh config.yml

