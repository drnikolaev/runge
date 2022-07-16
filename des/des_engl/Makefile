SHELL=/bin/sh

ROOT_PATH=../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/suffixes.inc

SOURCES = $(wildcard *.f)
OBJS    = $(patsubst %.f, $(OBJDIR)/%.o, $(SOURCES))
DOBJS   = $(patsubst %.f, $(DOBJDIR)/%.o, $(SOURCES))

FCOMMON_FLAGS += -fPIC

OBJDIR  =$(call concat,./obj,$(NF),$(NM),$(NE),$(NI))
DOBJDIR =$(call concat,./dobj,$(NF),$(NM),$(NE),$(NI))
LIBS   +=$(call concat,-lcvm,$(NE),$(NI))
DLIBS  +=$(call concat,-lcvm,$(NE),$(NI),_debug)
SONAME  =$(call concat,libdes_engl,$(NE),$(NI),.so)
SODNAME =$(call concat,libdes_engl,$(NE),$(NI),_debug.so)
DYNAME  =$(call concat,libdes_engl,$(NE),$(NI),.dylib)
DYDNAME =$(call concat,libdes_engl,$(NE),$(NI),_debug.dylib)

include $(INC_PATH)/libs.inc

ifdef IFORT
  include $(INC_PATH)/ifort.inc
endif

# FCOMMON_FLAGS += -Wall
ifdef PROF
  FCOMMON_FLAGS += -pg
endif

SOFLAGS  = $(FCOMMON_FLAGS) -O3 -DCVM_EXPORTS -fPIC
SOFLAGSD = $(FCOMMON_FLAGS) -g -DCVM_EXPORTS -fPIC
LFLAGS  += $(COMMON_LD_FLAGS) -L$(OUTDIR) -shared
LFLAGSD += $(COMMON_LD_FLAGS) -L$(OUTDIR) -shared

all : release debug
ifdef MAC
  release: $(OUTDIR)/$(DYNAME)
  debug:   $(OUTDIR)/$(DYDNAME)
else
  release: $(OUTDIR)/$(SONAME)
  debug:   $(OUTDIR)/$(SODNAME)
endif

$(OUTDIR)/$(SONAME): $(OBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGS) -Wl,-soname,$(SONAME) -o $@ $^ -Wl,--start-group $(LIBS) -Wl,--end-group

$(OUTDIR)/$(SODNAME): $(DOBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGSD) -Wl,-soname,$(SODNAME) -o $@ $^ -Wl,--start-group $(DLIBS) -Wl,--end-group

$(OUTDIR)/$(DYNAME): $(OBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGS) -Wl,-dylib -o $@ $^ $(LIBS)

$(OUTDIR)/$(DYDNAME): $(DOBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Linking: $@
	$(AT)$(F77) $(LFLAGSD) -Wl,-dylib -o $@ $^ $(DLIBS)

$(OBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(ECHO) Compiling release: $<
	$(AT)$(F77) $(SOFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.f
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(ECHO) Compiling debug: $<
	$(AT)$(F77) $(SOFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)rm -rf $(OBJDIR) $(DOBJDIR) $(OUTDIR)/$(SONAME) $(OUTDIR)/$(SODNAME)

