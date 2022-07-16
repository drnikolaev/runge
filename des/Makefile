SHELL=/bin/sh

ROOT_PATH=..
INC_PATH=$(ROOT_PATH)/makes
include $(INC_PATH)/makecmd.inc

all: debug release

.PHONY: debug
debug:
	make debug -C des_engl $(MAKECMD)
	make debug -C des_lou/mexp $(MAKECMD)
	make debug -C des_lou $(MAKECMD)
	make debug -C des_ros/inv $(MAKECMD)
	make debug -C des_ros $(MAKECMD)

.PHONY: release
release:
	make release -C des_engl $(MAKECMD)
	make release -C des_lou/mexp $(MAKECMD)
	make release -C des_lou $(MAKECMD)
	make release -C des_ros/inv $(MAKECMD)
	make release -C des_ros $(MAKECMD)

clean:
	make clean -C des_engl $(MAKECMD)
	make clean -C des_lou/mexp $(MAKECMD)
	make clean -C des_lou $(MAKECMD)
	make clean -C des_ros/inv $(MAKECMD)
	make clean -C des_ros $(MAKECMD)
