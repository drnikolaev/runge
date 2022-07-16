SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/makecmd.inc

ifdef VERBOSE
AT   =
else
AT   = @
endif

AR   = ar cr
ECHO = $(AT)echo
ifdef ICCT
  CC=icpc
else
  CC=g++
endif

DOBJDIR=./dobj
OBJDIR=./obj
CVM_SRC_PATH=../../cvm/src
#DOUTDIR=./Debug

ifdef EM64T
  OUTDIR=../../lib64
  DOUTDIR=../../lib64
  CVM_LIB_PATH=../../lib64
#  LIBS=-lgfortran -lblas_em64t -llapack_em64t
#  DLIBS=-lgfortran -lblas_em64t_debug -llapack_em64t_debug
  LIBS=-lgfortran
  DLIBS=-lgfortran
else 
  OUTDIR=../../lib
  DOUTDIR=../../lib
  CVM_LIB_PATH=../../lib
#  LIBS=-lgfortran -lblas_ia32 -llapack_ia32
#  DLIBS=-lgfortran -lblas_ia32_debug -llapack_ia32_debug
  LIBS=-lgfortran
  DLIBS=-lgfortran
endif

ifdef COMSPEC
  STATIC_ONLY=1
  COMMON_LFLAGS = -L$(OUTDIR) 
else
  LIBS += -ldl -lpthread
  DLIBS += -ldl -lpthread
endif

COMMON_FLAGS = -Wall -std=c++0x -I$(CVM_SRC_PATH)
COMMON_LFLAGS = -L$(CVM_LIB_PATH)


ifdef EM64T
  ifdef ILP64
    LIBS  += -lcvm_em64t_ilp64
    DLIBS += -lcvm_em64t_ilp64_debug
    OUTNAME_RELEASE=$(OUTDIR)/regtest_des_em64t_ilp64
    OUTNAME_DEBUG=$(OUTDIR)/regtest_des_em64t_ilp64_debug
  else
    LIBS  += -lcvm_em64t
    DLIBS += -lcvm_em64t_debug
    OUTNAME_RELEASE=$(OUTDIR)/regtest_des_em64t
    OUTNAME_DEBUG=$(OUTDIR)/regtest_des_em64t_debug
  endif
else
  LIBS  += -lcvm_ia32
  DLIBS += -lcvm_ia32_debug
  OUTNAME_RELEASE=$(OUTDIR)/regtest_des_ia32
  OUTNAME_DEBUG=$(OUTDIR)/regtest_des_ia32_debug
endif

#SOURCES=$(wildcard *.cpp)
SOURCES=test_des.cpp
SOURCES+=../loader.cpp
SOURCES+=../solver.cpp
#OBJS=$(patsubst %.cpp, $(OBJDIR)/%.o, $(SOURCES))
#DOBJS=$(patsubst %.cpp, $(DOBJDIR)/%.o, $(SOURCES))
OBJS=$(OBJDIR)/test_des.o $(OBJDIR)/loader.o $(OBJDIR)/solver.o
DOBJS=$(DOBJDIR)/test_des.o $(DOBJDIR)/loader.o $(DOBJDIR)/solver.o

include $(INC_PATH)/suffixes.inc

CFLAGS=$(COMMON_FLAGS)
CFLAGSD=$(COMMON_FLAGS) -g -DDEBUG
LFLAGS=$(COMMON_LFLAGS)
LFLAGSD=$(COMMON_LFLAGS) -g

ifdef PROF
  CFLAGSD += -pg
  LFLAGSD += -pg
endif

#$(call make-depend,source-file,object-file,depend-file)
define make-depend
  $(AT)$(CC) -MM -MF $3 -MP -MT $2 $(COMMON_FLAGS) $1
endef

all: debug release
release : $(OUTNAME_RELEASE)
debug : $(OUTNAME_DEBUG)

ifdef MAC
$(OUTNAME_RELEASE) : $(OBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(OBJS) $(LFLAGS) $(LIBS)

$(OUTNAME_DEBUG) : $(DOBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(DOBJS) $(LFLAGSD) $(DLIBS)
else
$(OUTNAME_RELEASE) : $(OBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(OBJS) $(LFLAGS) -Wl,--start-group $(LIBS) -Wl,--end-group

$(OUTNAME_DEBUG) : $(DOBJS)
	$(ECHO) Linking: $@
	$(AT)$(CC) -o $@ $(DOBJS) $(LFLAGSD) -Wl,--start-group $(DLIBS) -Wl,--end-group
endif

$(OBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGS) -c -o $@ $<

$(OBJDIR)/%.o: ../%.cpp
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGSD) -c -o $@ $<

$(DOBJDIR)/%.o: ../%.cpp
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)-rm -rf $(OBJDIR) $(DOBJDIR) $(OUTDIR)/test_des* $(DOUTDIR)/test_des*

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(OBJDIR)/*.d $(DOBJDIR)/*.d
endif

