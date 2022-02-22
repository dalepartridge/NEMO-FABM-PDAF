#!/bin/bash
#SBATCH --time=08:00:00
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=20
#SBATCH --threads-per-core=1
#SBATCH --job-name=ORCA2
#SBATCH --partition=all
#SBATCH --mail-type=ALL
#SBATCH --mail-user=momm
#SBATCH --exclusive


# NOTE:
# the simulation year and month must be passed to this pbs script at runtime
# by the command line, e.g.:
# "sbatch 1980 1 MonthlyChainHindcast.sh"
#

module load intel
module load intel-mpi
module load hdf5-intelmpi
module load netcdf-intelmpi

export I_MPI_PMI_LIBRARY=/usr/lib64/libpmi.so
export I_MPI_PIN_PROCS=0-19
export I_MPI_EXTRA_FILESYSTEM=on
export I_MPI_EXTRA_FILESYSTEM_LIST=gpfs
export OMP_NUM_THREADS=1
ulimit -s 10240
#ulimit -s unlimited

set -u #break on unset variables

RUNDIR=/work/momm/ORCA2
cd $RUNDIR

CORES=$(( SLURM_NTASKS ))

#cat > ./xios.conf <<EOL
#$COMPUTETAG  ./nemo.exe
#$XIOSTAG ./xios_server.exe
#EOL

#echo "Total number of nodes / cores used:" $SLURM_NNODES "/" $CORES
#echo "Compute cores used: $COMPUTECORES"
#echo "XIOS cores used:" $XIOSCORES
#echo "Buffer cores blocked for XIOS:" $XIOSBLOCKEDCORES

#cat > xios.conf <<EOL
#0-359  ./nemo.exe
#360    ./xios_server.exe
#EOL

#if [ $stopflag -ne 0 ]
#then
#   echo "Not ready to launch. Forced exit."
#   exit
#fi

echo "Launching NEMO at $(date +%s) seconds since 1970-01-01 00:00:00"

#srun -K1 -n $(( COMPUTECORES + XIOSCORES )) -m plane=20 --multi-prog ./xios.conf
srun -K1 -n $CORES ./nemo.exe

echo "Finished NEMO at $(date +%s) seconds since 1970-01-01 00:00:00"

