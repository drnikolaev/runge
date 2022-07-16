SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/suffixes.inc
include $(INC_PATH)/matlab.inc

ifdef EM64T
  SONAME=odeEngland.mexa64
else
  SONAME=odeEngland.mexglx
endif

# solver library:
LIBS   +=$(call concat,-ldes_engl,$(NE),$(NI))
DLIBS  +=$(call concat,-ldes_engl,$(NE),$(NI),_debug)

include $(INC_PATH)/mex.inc

