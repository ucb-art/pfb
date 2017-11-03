export RISCV=/tools/projects/stevo/acmes/riscv-install
export PATH=$PATH:$RISCV/bin

export LD_LIBRARY_PATH=~rigge/gcc/lib64:~rigge/gcc/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=$RISCV/lib64:$RISCV/lib:$LD_LIBRARY_PATH

# synopsys vcs, also for dve
export PATH=/tools/synopsys/vcs/M-2017.03/bin:$PATH
export VCS_HOME=/tools/synopsys/vcs/M-2017.03/
export VCS_64=1

# memory compiler
export INTERRAD_LICENSE_FILE=/tools/commercial/interra/flexlm/license_N28.dat
export TSMCHOME=/tools/tstech16/CLN16FFC/TSMCHOME

# temporary, hopefully
export MC2_INSTALL_DIR=~stevo.bailey/mc2/MC2_2013.12.00.f

export PATH=$PATH:$MC2_INSTALL_DIR/bin

# cadence incisive
export PATH=/tools/cadence/INCISIV/INCISIVE152/tools/bin:$PATH

# cadence tools
export PATH=$PATH:/tools/cadence/GENUS/GENUS162/tools/bin:/tools/cadence/INNOVUS/INNOVUS162/tools/bin:/tools/cadence/INCISIV/INCISIVE152/tools/bin

# layer props for calibre (currently not working?)
# alias viewgds="calibredrv -dl /users/stevo.bailey/TSMC16.layerprops -s /users/bmzimmer/.calibrewb_workspace/wbinit.tcl -m "

# java 8
export MY_JDK=jdk1.8.0_144
export JAVA_HOME=/users/rigge/$MY_JDK/
export PATH=/users/rigge/$MY_JDK/bin:$PATH
export LD_LIBRARY_PATH=/users/rigge/$MY_JDK/lib:$LD_LIBRARY_PATH
export MANPATH=/users/rigge/$MY_JDK/man:$MANPATH

# verilator
export PATH=$PATH:/tools/projects/stevo/craft/craft2-chip/verisim/verilator/install/bin

# get the right gcc 
scl enable devtoolset-2 bash

