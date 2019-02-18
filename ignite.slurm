#!/bin/bash

#SBATCH --mail-user=shawn.garbett@vumc.org
#SBATCH --mail-type=FAIL
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --time=3:00:00
#SBATCH --mem=12G
#SBATCH --output=status/job%a.out
#SBATCH --array=1-9800
#SBATCH --constrain=haswell

module purge
module load Intel/2017.4.196
module load IntelMPI/2017.3.196
module load R/3.4.3

Rscript --no-save sim-accre.R ${SLURM_ARRAY_TASK_ID}
