HEADERS += ../loader.h \
    ../solver.h \
    ./solutions.h \
    ./engine.h \
    ./runge.h \
    ./drawer.h \
    ./drawersettings.h \
    ./drawer3dsettings.h \
    ./glwidget.h \
    ./orbit.h \
    ./orbithandle.h \
    ./view2d.h \
    ./scene2d.h \
    ./drawer3d.h \
    ./lock.h \
    ./utils.h \
    ./exporttoimage.h \
    ./printto.h \
    ./colordialog.h \
    ./about.h


SOURCES += ../loader.cpp \
    ../solver.cpp \
    ./engine.cpp \
    ./main.cpp \
    ./runge.cpp \
    ./solutions.cpp \
    ./drawer.cpp \
    ./drawersettings.cpp \
    ./drawer3dsettings.cpp \
    ./glwidget.cpp \
    ./orbit.cpp \
    ./orbithandle.cpp \
    ./view2d.cpp \
    ./scene2d.cpp \
    ./drawer3d.cpp \
    ./utils.cpp \
    ./exporttoimage.cpp \
    ./printto.cpp \
    ./colordialog.cpp \
    ./about.cpp


FORMS += ./runge.ui \
    ./drawer.ui \
    ./drawer3d.ui \
    ./drawersettings.ui \
    ./drawer3dsettings.ui \
    ./exporttoimage.ui \
    ./printto.ui \
    ./about.ui

RESOURCES += runge.qrc
