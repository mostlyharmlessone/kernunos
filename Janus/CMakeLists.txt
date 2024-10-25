cmake_minimum_required(VERSION 3.30)
cmake_policy(VERSION 3.30)
set (CMAKE_Fortran_COMPILER_ID "gfortran")
set (CMAKE_Fortran_COMPILER "gfortran")
set (CMAKE_C_COMPILER "gcc")
# these don't override system environment, unfortunately
set (FC "gfortran")
set (CXX "g++")
set (CC "gcc")

project(kernunos VERSION 0.1.0 LANGUAGES C CXX Fortran)
set(CMAKE_MODULE_PATH cmake)
include(cmake/FortranEnvironment.cmake)
enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
# /opt/cuda/bin/nvcc is the compiler
#enable_language(ASM)

#set(CMAKE_CROSSCOMPILING true)
#set(CMAKE_SYSTEM_NAME Windows)
#set(CMAKE_SYSTEM_VERSION 10.0)

set(CMAKE_CXX_STANDARD 17) # doesn't apply to win32
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)
# should read again how to manage compiler flags with Cmake
if(CMAKE_Fortran_COMPILER MATCHES "gfortran")
    set(dialect "-std=f2018 -cpp -ffree-line-length-none -fall-intrinsics")
#     set(dialect "-std=gnu")
     set(bounds "-fcheck=bounds")
#     set(bounds "-fcheck=all -pedantic")
     set(omp "-fopenmp")
     set(NaN "-Wall -Wextra -Wimplicit-interface -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-99999999")
#  profiling with gprof 
 #     set(-pg)    
#  src profiling with gcov -a -b xxx.f90
#     set(-g -coverage) 
#     set(-Wno-deprecated-declarations)
    set (bugcheck "-fno-strict-aliasing -fwrapv -fno-aggressive-loop-optimizations -fsanitize=undefined")
    set(optimize "-O2 -fexternal-blas")
endif()
if(CMAKE_Fortran_COMPILER MATCHES "Intel")
    set(dialect "-stand f08 -free -implicitnone")
    set(bounds "-check bounds")
endif()
#  nvfortran==pgi
if(CMAKE_Fortran_COMPILER MATCHES "nvfortran")
#    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
#    set(bounds "-C")
endif()

# needed for qt
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

#if(CMAKE_VERSION VERSION_LESS "3.7.0")
    set(CMAKE_INCLUDE_CURRENT_DIR ON)    
#endif

#set(CMAKE_BUILD_TYPE RELEASE) # no debug or bounds check
set(CMAKE_BUILD_TYPE DEBUG) #this include debugging with addition of bounds flags as specified below

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${omp}")

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${NaN}")
#set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bugcheck}")

#set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${optimize}")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${bounds}")
set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${NaN}")

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 0) #set to 1 to show dependencies

set (CMAKE_PREFIX_PATH "/usr/local/lib")

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

if(WIN32)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
endif(WIN32)

# STATIC stuff (Windows)
set(BUILD_FOR_WIN FALSE)
if (${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        set(BUILD_FOR_WIN TRUE)
endif()
option(STATIC_BUILD "Build a static binary." ${BUILD_FOR_WIN})

if (STATIC_BUILD)
        set(CMAKE_EXE_LINKER_FLAGS "-static")
        set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" CONFIG)
        set(BUILD_SHARED_LIBS OFF)
endif()

if (STATIC_BUILD AND ${CMAKE_SYSTEM_NAME} MATCHES "Windows")
        # include plugins into static build on windows
        # (we lack support for static on other platforms right now)
        set(QT_PLUGINS SvgIcon WindowsIntegration WindowsVistaStyle)
endif()

link_directories(${CMAKE_SOURCE_DIR}/lib)

# find the required packages
#find_package(GLM REQUIRED)
#message(STATUS "Found GLM ")
#find_package(GLFW REQUIRED)
#message(STATUS "Found GLFW ")
#set(EPOXY_ROOT_DIR ${LIBDIR}/libepoxy)
#find_package(LibEpoxy REQUIRED)
#include_directories(${LibEpoxy_INCLUDE_DIRS})
#message(STATUS "Found LibEpoxy in ${LIBEPOXY_LIBRARIES}")
#if(UNIX AND NOT APPLE)
# set(wxWidgets_CONFIG_OPTIONS_DEFAULT "--toolkit=gtk3")
#endif()
#if (BUILD_FOR_WIN)
#nothing
#else()
# find_package(wxWidgets 3 REQUIRED gl richtext aui adv html webview core xml net base)
# find_package(wxWidgets 3 REQUIRED gl core net base)
#endif()
#if(wxWidgets_FOUND)
# include(${wxWidgets_USE_FILE})
#      message("-- FOUND WxWidgets library")
#else()
#    message("Failed to find WxWidgets library")
#endif()

# Qt
#SET(Qt5_DIR "/usr/lib/x86_64-linux-gnu/qt5")
SET(Qt6_DIR "/usr/lib/x86_64-linux-gnu/qt6")
#find_package(Qt5 REQUIRED COMPONENTS Core Gui Widgets OpenGL)
#find_package(Qt6 REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools)
find_package(QT NAMES Qt6 REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools)
find_package(Qt${QT_VERSION_MAJOR} REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Concurrent REQUIRED)

set(TS_FILES src/cxx/kernunos/kernunos_en_US.ts)

#GTK4
find_package (PkgConfig REQUIRED)
pkg_check_modules(GTK4 REQUIRED gtk4)
INCLUDE_DIRECTORIES(${GTK4_INCLUDE_DIRS})
LINK_DIRECTORIES (${GTK4_LIBRARY_DIRS})
#GTK3
pkg_check_modules(GTK3 REQUIRED gtk+-3.0)
message(STATUS "----- GTK3_INCLUDE_DIRS: ${GTK3_INCLUDE_DIRS}")
message(STATUS "----- GKT3_LIBRARIES: ${GTK3_LIBRARIES}")
message(STATUS "----- GTK3_LINK_LIBRARIES: ${GTK3_LINK_LIBRARIES}")
include_directories(${GTK3_INCLUDE_DIRS})
link_directories(${GTK3_LIBRARY_DIRS})
list(APPEND FC_DEP_LIBS ${GTK3_LIBRARIES})

find_library(GFORTRAN_LIBRARY NAMES gfortran)  # trying to fix ISO_Fortran_binding.h not found error, doesn't solve it, might be Intel compiler only
ADD_COMPILE_DEFINITIONS(ASSIMP_VERSION="${ASSIMP_VERSION}")
find_package(BLAS)
 if ( BLAS_FOUND )
  message(STATUS "BLAS found at ${BLAS_LIBRARIES}")
 else()
  set(BLAS_LIBRARIES "/usr/lib/libblas.so")
 endif()
find_package(LAPACK)
 if ( LAPACK_FOUND )
  message(STATUS "LAPACK found at ${LAPACK_LIBRARIES}")
 else()
  set(LAPACK_LIBRARIES "/usr/lib/liblapack.so.3")
 endif()
find_package(SuperLU)
 if ( SUPERLU_FOUND )
  message(STATUS "SUPERLU found at ${SUPERLU_LIBRARIES}")   
 else()
  set(SUPERLU_LIBRARIES "/usr/libsuperlu.so")
 endif()
find_package(OPENCL)
 if ( OPENCL_FOUND )
  message(STATUS "OPENCL found at ${OPENCL_LIBRARIES}")   
 else()
#  set(OPENCL_LIBRARIES "/usr/lib/libOpenCL.so")
  set(OPENCL_LIBRARIES "/usr/lib/libOpenCL.so.1") 
 endif()
find_package(OpenMP)
#  boost, needed for gnuplot-iostream
find_package(Boost REQUIRED COMPONENTS iostreams system filesystem)

# from learnopengl: attempt to make buildable apple/win32 too
if(APPLE)
  find_package(Freetype REQUIRED)
  message(STATUS "Found Freetype in ${FREETYPE_INCLUDE_DIRS}")
endif(APPLE)
if(WIN32)
  set(LIBS glfw3 opengl32 freetype irrKlang)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS)
elseif(UNIX AND NOT APPLE)
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")
 find_package(OpenGL REQUIRED)
  if ( OPENGL_FOUND )
  message(STATUS "OPENGL found at ${OPENGL_LIBRARIES}")   
  add_definitions(${OPENGL_DEFINITIONS})
 endif()
  find_package(X11 REQUIRED)
  set(LIBS ${GLFW3_LIBRARY} X11 Xrandr Xinerama Xi Xxf86vm Xcursor GL dl pthread freetype )
  set (CMAKE_CXX_LINK_EXECUTABLE "${CMAKE_CXX_LINK_EXECUTABLE} -ldl")
elseif(APPLE)
  INCLUDE_DIRECTORIES(/System/Library/Frameworks)
  FIND_LIBRARY(COCOA_LIBRARY Cocoa)
  FIND_LIBRARY(OpenGL_LIBRARY OpenGL)
  FIND_LIBRARY(IOKit_LIBRARY IOKit)
  FIND_LIBRARY(CoreVideo_LIBRARY CoreVideo)
  MARK_AS_ADVANCED(COCOA_LIBRARY OpenGL_LIBRARY)
  SET(APPLE_LIBS ${COCOA_LIBRARY} ${IOKit_LIBRARY} ${OpenGL_LIBRARY} ${CoreVideo_LIBRARY})
  SET(APPLE_LIBS ${APPLE_LIBS} ${GLFW3_LIBRARY} ${FREETYPE_LIBRARIES})
  set(LIBS ${LIBS} ${APPLE_LIBS})
else()
  set(LIBS )
endif(WIN32)

#   Fortran Source
    file(GLOB_RECURSE lib_sources90  src/src90 *.f90 *.for)
    file(GLOB_RECURSE mod_sources90  src/mod90 *.f90)
    add_library( rply_includes ${rply_sources})
    add_library( modules ${mod_sources90} ${interface})
    add_library( splia ${lib_sources90} ${interface})

#superlu example from chapter12

set(LIBS2 ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${SUPERLU_LIBRARIES})
set(HH_SOURCES
                   src/HansonHopkins/driverPiecewiseLinear.f90
                   src/HansonHopkins/sparseAssign.f90
                   src/HansonHopkins/sparseOps.f90
                   src/HansonHopkins/sparseSort.f90
                   src/HansonHopkins/sparseTypes.f90
                   src/HansonHopkins/sparseUtils.f90
                   src/HansonHopkins/sluInterop.f90
                   src/HansonHopkins/sortElements.f90
                   src/HansonHopkins/fortran_double_call_SuperLU.c
                   src/HansonHopkins/slu_Cnames.h
                   src/HansonHopkins/slu_ddefs.h
                   src/HansonHopkins/slu_util.h
                   src/HansonHopkins/superlu_enum_consts.h
                   src/HansonHopkins/supermatrix.h
                   )
               add_executable(driverPiecewiseLinear
                   ${HH_SOURCES}
               )
target_link_libraries(driverPiecewiseLinear modules ${LIBS2})

#viennaCL

# ViennaCL example
#  add_executable(viennacl src/cxx/vienna/viennacl-info.cpp)  
#  target_link_libraries(viennacl ${OPENCL_LIBRARIES} ) 
#  add_executable(sparse src/cxx/vienna/sparse.cpp)  
#  target_link_libraries(sparse ${OPENCL_LIBRARIES} )
#  add_executable(blasexample src/cxx/vienna/blas3.cpp)  
#  target_link_libraries(blasexample ${OPENCL_LIBRARIES} )

# Compile.

#   PLY conversion from  https://w3.impa.br/~diego/software/rply/
#                        https://github.com/diegonehab/rply
#   copy convert.c from rply/etc to rply and both to src/c/rply
#    file(COPY rply/etc/convert.c DESTINATION src/c/rply)
#    file(COPY rply/rply.c rply/rply.h rply/rplyfile.h DESTINATION src/c/rply)
#    file(GLOB_RECURSE src/c/rply *.c .h)
#    add_executable(ConvertPLYtoBIN src/c/rply/convert.c src/c/rply/rply.c )

#   C source 
#   rply conversion 
    file(GLOB rply_sources  src/c/rply/convertplytobin.c src/c/rply/rply.c src/c/rply *.h )

    file(GLOB interface src/c/kernunos/logc.c src/c/kernunos/logc.h
        src/c/kernunos/Ccounter.c src/c/kernunos/counter.h)

    # gtk example with gtk4 & blas fortran
    #doesn't compile with mingw cmake because of multiple definitions of dgemm
    #     file(GLOB blas_f2c src/c/gtk/c_dgemm.f90)
#     add_library( blas_f2c ${blas_f2c})
#     add_executable(example-2 src/c/gtk/gtkexample2.c)
#     target_link_libraries(example-2 ${GTK4_LIBRARIES} blas_f2c ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
#   gtk GL example
#    add_executable(glarea src/c/gtk/glarea-app-window.c )
#    target_link_libraries(glarea ${GTK3_LIBRARIES} epoxy)

    
#libraries      
    set(LIBS ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES} ${GFORTRAN_LIBRARY})

# need this so that GLwidget.h can be found by AUTOUIC and ui.mainwindow.h, and for rply and assimp
include_directories(src/cxx/kernunos src/c/rply) # /usr/local/include/coretran)

#kernunos Qt version


set(PROJECT_SOURCES
                   src/cxx/kernunos/kernunos.cpp src/cxx/kernunos/kernunos.h
                   src/cxx/kernunos/GLwidget.cpp src/cxx/kernunos/GLwidget.h
                   src/cxx/kernunos/assistant.cpp src/cxx/kernunos/assistant.h
                   src/c/kernunos/logc.c src/c/kernunos/logc.h
                   src/c/kernunos/Ccounter.c src/c/kernunos/counter.h
                   src/cxx/kernunos/gnuplot_lioc.cpp
                   src/cxx/kernunos/gnuplot_load.cpp
                   gnuplot-iostream/gnuplot-iostream.h
                   src/cxx/kernunos/mainwindow.ui
                   src/c/kernunos/ConvertPLYtoBIN.c
                   src/src90/ConvertOFFtoSTL_C.f90
                   src/src90/janus.f90 
                   ${TS_FILES}
                   )

if(${QT_VERSION_MAJOR} GREATER_EQUAL 6)
    qt_add_executable(kernunos
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES}
  )
# Define target properties for Android with Qt 6 as:
#    set_property(TARGET kernunos APPEND PROPERTY QT_ANDROID_PACKAGE_SOURCE_DIR
#                 ${CMAKE_CURRENT_SOURCE_DIR}/android)
# For more information, see https://doc.qt.io/qt-6/qt-add-executable.html#target-creation

    qt_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})
else()
    if(ANDROID)
        add_library(kernunos SHARED
            ${PROJECT_SOURCES}
        )
# Define properties for Android with Qt 5 after find_package() calls as:
#    set(ANDROID_PACKAGE_SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/android")
    else()
        add_executable(kernunos
            ${PROJECT_SOURCES}
        )
    endif()

    qt5_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})
endif()


set_target_properties(kernunos PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER my.example.com
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

# Target for gnuplot-iostream
add_library(gnuplot_iostream INTERFACE)
target_include_directories(gnuplot_iostream INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/>
    $<INSTALL_INTERFACE:>)
target_link_libraries(gnuplot_iostream INTERFACE
    Boost::iostreams
    Boost::system
    Boost::filesystem)

target_link_libraries(kernunos
               PRIVATE OpenMP::OpenMP_CXX Qt6::Concurrent
               gnuplot_iostream
               boost_iostreams
               boost_system
               boost_filesystem
               PUBLIC Qt${QT_VERSION_MAJOR}::Core
                      Qt${QT_VERSION_MAJOR}::OpenGLWidgets
                      Qt${QT_VERSION_MAJOR}::Gui
                      Qt${QT_VERSION_MAJOR}::Widgets
                      Qt${QT_VERSION_MAJOR}::OpenGL
                      assimp splia modules rply_includes ${LIBS}
           )


if(NOT DEFINED INSTALL_EXAMPLESDIR)
    set(INSTALL_EXAMPLESDIR "/Janus/Janus")
endif()

install(TARGETS kernunos RUNTIME DESTINATION "${INSTALL_EXAMPLEDIR}" 
                BUNDLE DESTINATION "${INSTALL_EXAMPLEDIR}" 
                LIBRARY DESTINATION "${INSTALL_EXAMPLEDIR}")

if(QT_VERSION_MAJOR EQUAL 6)
    qt_finalize_executable(kernunos)
endif()


            #if (BUILD_FOR_WIN)
            #nothing
            #else()
            #Jupiter wxWidgets version
            #   add_executable(${PROJECT_NAME}
            #       src/cxx/Jupiter/Jupiter.cpp
            #       src/cxx/Jupiter/Jupiter.h
            #       src/src90/janus.f90 src/cxx/opengl_show.cpp
            #       src/cxx/Jupiter/mathstuff.cpp
            #       src/cxx/Jupiter/mathstuff.h
            #       src/cxx/Jupiter/camera.h
            #   )
            #   target_link_libraries(${PROJECT_NAME} PRIVATE OpenMP::OpenMP_CXX PUBLIC splia modules
            #       rply_includes ${wxWidgets_LIBRARIES} ${LIBS})
            #endif()
            #juno commandline/glfw version
            #    add_executable(juno
            #                   src/src90/juno.f90
            #                   src/src90/janus.f90
            #                   src/cxx/opengl_show.cpp
            #                   f90getopt/f90getopt.F90)
            #    target_link_libraries(juno
            #                   PRIVATE OpenMP::OpenMP_CXX
            #                   PUBLIC splia modules rply_includes  ${LIBS})

