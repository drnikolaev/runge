SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/suffixes.inc
include $(INC_PATH)/matlab.inc

ifdef EM64T
  SONAME=odeRosenbrockA.mexa64
else
  SONAME=odeRosenbrockA.mexglx
endif

# solver library:
LIBS   +=$(call concat,-ldes_ros,$(NE),$(NI))
DLIBS  +=$(call concat,-ldes_ros,$(NE),$(NI),_debug)

include $(INC_PATH)/mex.inc

