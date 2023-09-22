#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=92G
#SBATCH --time=24:00:00
#SBATCH --array=1-11
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=cv_w

module load StdEnv/2020 gcc/9.3.0 r/4.2.2

Rscript 1_cv_varsel.R $SLURM_ARRAY_TASK_ID willingness loo 8
