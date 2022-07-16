SHELL=/bin/sh

ROOT_PATH=..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/makecmd.inc

all: release

.PHONY: debug
debug:
	make debug -C odeLawson $(MAKECMD)
	make debug -C odeLawsonA $(MAKECMD)
	make debug -C odeLawsonU $(MAKECMD)
	make debug -C odeEngland $(MAKECMD)
	make debug -C odeRosenbrock $(MAKECMD)
	make debug -C odeRosenbrockA $(MAKECMD)

.PHONY: release
release:
	make release -C odeLawson $(MAKECMD)
	make release -C odeLawsonA $(MAKECMD)
	make release -C odeLawsonU $(MAKECMD)
	make release -C odeEngland $(MAKECMD)
	make release -C odeRosenbrock $(MAKECMD)
	make release -C odeRosenbrockA $(MAKECMD)

clean:
	make clean -C odeLawson $(MAKECMD)
	make clean -C odeLawsonA $(MAKECMD)
	make clean -C odeLawsonU $(MAKECMD)
	make clean -C odeEngland $(MAKECMD)
	make clean -C odeRosenbrock $(MAKECMD)
	make clean -C odeRosenbrockA $(MAKECMD)

