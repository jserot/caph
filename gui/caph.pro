QT       += core gui
QT 		 += xml
QT 		 += widgets

//CONFIG -= c++11

TARGET = caph
TEMPLATE = app

win32 {
LIBS += -L /cygdrive/c/MinGW/lib
}

SOURCES += main.cpp\
        mainwindow.cpp \
    options.cpp \
    project.cpp \
    project_window.cpp \
    config.cpp \
    syntax_highlighter.cpp \ 
    option.cpp \
    command.cpp

HEADERS  += mainwindow.h \
    options.h \
    app_file.h \
    project.h \
    project_window.h \
    config.h \
    syntax_highlighter.h \
    command.h

FORMS    += uis/mainwindow.ui \
    uis/projet.ui \
    uis/config.ui

RESOURCES += \
    resources.qrc
































