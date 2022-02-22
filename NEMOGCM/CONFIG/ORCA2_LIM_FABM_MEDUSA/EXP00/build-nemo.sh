#!/bin/bash 

module load intel intel-mpi netcdf-intelmpi hdf5-intelmpi

NEMO_BUILD_DIR=$HOME/git/NEMO-shelf/NEMOGCM/CONFIG
RUNDIR=~/build/ORCA2
export XIOS_HOME=$HOME/local/xios-intel
export FABM_HOME=$HOME/local/fabm/nemo

ARCH=CETO_INTEL_NOSIGNEDZERO

cd $NEMO_BUILD_DIR
echo "Cleaning old build ..."
rm -f $RUNDIR/nemo.exe $NEMO_BUILD_DIR/ORCA2_LIM_FABM_BLD_SCRATCH/BLD/bin/nemo.exe
./makenemo -m $ARCH -n ORCA2_LIM_FABM_BLD_SCRATCH clean_config

echo "Building NEMO-FABM..."
./makenemo -m $ARCH -r ORCA2_LIM_FABM_ERSEM -n ORCA2_LIM_FABM_BLD_SCRATCH | tee compile.log && mv $NEMO_BUILD_DIR/ORCA2_LIM_FABM_BLD_SCRATCH/BLD/bin/nemo.exe $RUNDIR/ && echo "Done."
