SHELL=/bin/sh

ROOT_PATH=../../..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/defines.inc
include $(INC_PATH)/suffixes.inc

COMMON_FLAGS+=-fPIC -std=c++0x -I$(ROOT_PATH)/cvm/src

SOURCES=$(wildcard *.cpp)
OBJS =$(patsubst %.cpp, $(OBJDIR)/%.o, $(SOURCES))
DOBJS=$(patsubst %.cpp, $(DOBJDIR)/%.o, $(SOURCES))

OBJDIR   =$(call concat,./obj,$(NC),$(NE),$(NI))
DOBJDIR  =$(call concat,./dobj,$(NC),$(NE),$(NI))
LIBNAME  =$(call concat,libmexp,$(NC),$(NE),$(NI),.a)
LIBDNAME =$(call concat,libmexp,$(NC),$(NE),$(NI),_debug.a)

CFLAGS=$(COMMON_FLAGS) -O3
CFLAGSD=$(COMMON_FLAGS) -g -DDEBUG
ifdef PROF
  CFLAGSD += -pg
  LFLAGSD += -pg
endif

all: release debug
release: $(OUTDIR)/$(LIBNAME)
debug: $(OUTDIR)/$(LIBDNAME)

$(OUTDIR)/$(LIBNAME): $(OBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $(OBJS)

$(OUTDIR)/$(LIBDNAME): $(DOBJS)
	$(AT)if [ ! -d $(OUTDIR) ]; then mkdir $(OUTDIR); fi
	$(ECHO) Archiving: $@
	$(AT)$(AR) $@ $(DOBJS)

$(OBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(OBJDIR) ]; then mkdir $(OBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGS) -c -o $@ $<

$(DOBJDIR)/%.o: %.cpp
	$(AT)if [ ! -d $(DOBJDIR) ]; then mkdir $(DOBJDIR); fi
	$(call make-depend,$<,$@,$(subst .o,.d,$@))
	$(ECHO) Compiling: $<
	$(AT)$(CC) $(CFLAGSD) -c -o $@ $<

clean:
	$(ECHO) Cleaning...
	$(AT)rm -rf $(OBJDIR) $(DOBJDIR) $(OUTDIR)/$(LIBNAME) $(OUTDIR)/$(LIBDNAME)

ifneq "$(MAKECMDGOALS)" "clean"
  -include $(OBJDIR)/*.d $(DOBJDIR)/*.d
endif

