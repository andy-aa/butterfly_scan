#!/bin/bash
#
#PBS -N byttefly_scan
#PBS -l select=1:ncpus=10:mem=10gb:interconnect=1g
#PBS -l walltime=0:30:00
#PBS -o output.txt
#PBS -j oe

cd $HOME/byttefly/byttefly_scan
module load r/4.1.2-gcc/8.4.1
Rscript run.R