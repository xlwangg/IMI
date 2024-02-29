#!/bin/bash

#SBATCH -N 1
#SBATCH -o cancel_job.out
#SBATCH -e cancel_job.err

# Initialize (x=0 is base run, i.e. no perturbation; x=1 is state vector element=1; etc.)
x=0

# Create run directory for each state vector element so we can
# apply the perturbation to each
while [ $x -le 212 ]; do

  scancel 8730313_${x}
  # Increment
  x=$[$x+1]
  
done
