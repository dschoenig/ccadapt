#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=8
#SBATCH --mem=24G
#SBATCH --time=3:00:00
#SBATCH --array=1-11
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=varsel_u

module load StdEnv/2023 gcc/12.3 r/4.3.1

Rscript 1_models_varsel.R $SLURM_ARRAY_TASK_ID urgency 8
