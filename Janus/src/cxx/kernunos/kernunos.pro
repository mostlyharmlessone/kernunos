QT       += core gui widgets opengl openglwidgets

TARGET = kernunos
TEMPLATE = app

mac {
    CONFIG -= app_bundle
}

SOURCES += \
    kernunos.cpp \
    main.cpp \
    mainwindow.cpp


HEADERS  += \
    kernunos.h \
    mainwindow.h

RESOURCES += GLShaders.qrc

DISTFILES += \
    CMakeLists.txt \
    CMakeLists.txt.user \
    kernunos.pro.user \
    kernunos_en_US.ts

FORMS += \
    mainwindow.ui

