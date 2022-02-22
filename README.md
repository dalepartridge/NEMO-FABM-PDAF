# Running on ARCHER2

A build script is located in `BLD`. Please modify the variables at the top of this script to something appropriate for your local configuration.

To get access to a run directory for NEMO-FABM-PDAF on ARCHER2, please contact Stefano Ciavatta for details.

Once you have access to the run directory, please replace the XIOS XML configuration files for your simulation with files located in `CFG`. 
Please also add the namelist `namelist.pdaf` in `CFG` to your run directory. This namelist indicates how many ensemble members to run.

Don't forget to increase the node/core count of your run e.g. for a 2-member simulation, you should double the node/core count required for
a simulation without PDAF.
