#!/bin/bash
# Run master script (potentially on Arcus cluster)
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --cpus-per-task=1
#SBATCH --time=72:00:00
#SBATCH --job-name=dw-master
#SBATCH --partition=htc
#SBATCH --mail-type=ALL
#SBATCH --mail-user=matti.vuorre@oii.ox.ac.uk
module purge
module load R
R CMD BATCH --no-save make.R
