#!/bin/bash

NEMO_BUILD_DIR=$HOME/git/NEMO-shelf/NEMOGCM/CONFIG
RUNDIR=/data/C1D_PAPA
export XIOS_HOME=$HOME/local/xios-gnu
export FABM_HOME=$HOME/local/fabm/nemo

ARCH=GCC_UBUNTU

cd $NEMO_BUILD_DIR
echo "Cleaning old build ..."
rm -f $RUNDIR/nemo.exe $NEMO_BUILD_DIR/C1D_PAPA_FABM_BLD_SCRATCH/BLD/bin/nemo.exe
./makenemo -m $ARCH -n C1D_PAPA_FABM_BLD_SCRATCH clean_config

echo "Building NEMO-FABM..."
./makenemo -m $ARCH -r C1D_PAPA_FABM_ERSEM -n C1D_PAPA_FABM_BLD_SCRATCH | tee compile.log && mv $NEMO_BUILD_DIR/C1D_PAPA_FABM_BLD_SCRATCH/BLD/bin/nemo.exe $RUNDIR/ && echo "Done."
