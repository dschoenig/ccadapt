#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=18G
#SBATCH --time=0:20:00
#SBATCH --array=1-10
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=varsel

module load StdEnv/2020 gcc/9.3.0 r/4.2.2

Rscript 1_variable_selection.R $SLURM_ARRAY_TASK_ID
