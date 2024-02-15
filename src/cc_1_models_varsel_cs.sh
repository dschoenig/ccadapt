#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=48G
#SBATCH --time=24:00:00
#SBATCH --array=1-10
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=varsel_cs

module load StdEnv/2023 gcc/12.3 r/4.3.1

Rscript 1_models_varsel.R $SLURM_ARRAY_TASK_ID categorical.sd 8
