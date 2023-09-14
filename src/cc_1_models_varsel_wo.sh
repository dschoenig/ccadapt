#!/bin/bash
#SBATCH --account=def-cricrime 
#SBATCH --cpus-per-task=4
#SBATCH --mem=48G
#SBATCH --time=12:00:00
#SBATCH --array=1-11
#SBATCH --mail-user=schonig.daniel@courrier.uqam.ca
#SBATCH --mail-type=ALL
#SBATCH --job-name=varsel_wo

module load StdEnv/2020 gcc/9.3.0 r/4.2.2

Rscript 1_models_varsel.R $SLURM_ARRAY_TASK_ID willingness.ord
