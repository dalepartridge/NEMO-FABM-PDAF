#! /bin/sh -
set -eu

NEMO_BUILD_DIR=/home/n02/n02/nenb/NEMO-FABM-PDAF/NEMOGCM/CONFIG
RUNDIR=/work/n02/n02/nenb/H2020/hind.79-18.v2
ARCH=X86_ARCHER2-Cray-FABM
REF_CONFIG=ORCA1_LIM_FABM_MEDUSA_PDAF
MY_CONFIG=ORCA1_LIM_FABM_PDAF_BLD_SCRATCH

export XIOS_HOME=/home/n01/shared/gle/XIOS1
export FABM_HOME=/home/n01/shared/sciava/local/fabm/nemo-ARCHER2
export PDAF_HOME=/home/n02/n02/nenb/NEMO-FABM-PDAF/PDAF_V116

module -s restore /work/n01/shared/acc/n01_modules/ucx_env

cd $NEMO_BUILD_DIR

echo "Cleaning old build..."

rm -f $RUNDIR/nemo.exe $NEMO_BUILD_DIR/$MY_CONFIG/BLD/bin/nemo.exe
echo y | ./makenemo -m $ARCH -n $MY_CONFIG clean_config

echo "Adding PDAF attachments..."

if [ ! -d "./MY_SRC" ]; then
    mkdir MY_SRC
else
    rm -rf ./MY_SRC
    mkdir MY_SRC
fi
cp -a "../EXTERNAL/PDAF/pdaf_bindings/." ./MY_SRC/
cp -a "../EXTERNAL/PDAF/nemo_src/." ./MY_SRC/
CWD=`pwd`
export MY_SRC=${CWD}/MY_SRC

echo "Building NEMO-FABM..."

yes "" | ./makenemo -n $MY_CONFIG -e $MY_SRC -r $REF_CONFIG -m $ARCH -j 16 && mv $NEMO_BUILD_DIR/$MY_CONFIG/BLD/bin/nemo.exe $RUNDIR && echo "Done."

