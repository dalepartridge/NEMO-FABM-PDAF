#!/bin/bash 

module load intel intel-mpi netcdf-intelmpi hdf5-intelmpi

NEMO_BUILD_DIR=$HOME/git/NEMO-shelf/NEMOGCM/CONFIG
RUNDIR=/data/ORCA2
export XIOS_HOME=$HOME/local/xios-intel-debug
export FABM_HOME=$HOME/local/fabm/nemo-debug

ARCH=CETO_INTEL_NOSIGNEDZERO_DEBUG

cd $NEMO_BUILD_DIR
echo "Cleaning old build ..."
rm -f $RUNDIR/nemo-debug.exe $NEMO_BUILD_DIR/ORCA2_LIM_FABM_DBG_SCRATCH/BLD/bin/nemo.exe
./makenemo -m $ARCH -n ORCA2_LIM_FABM_DBG_SCRATCH clean_config

echo "Building NEMO-FABM..."
./makenemo -m $ARCH -r ORCA2_LIM_FABM_ERSEM -n ORCA2_LIM_FABM_DBG_SCRATCH | tee compile-debug.log && mv $NEMO_BUILD_DIR/ORCA2_LIM_FABM_DBG_SCRATCH/BLD/bin/nemo.exe $RUNDIR/nemo-debug.exe && echo "Done."
