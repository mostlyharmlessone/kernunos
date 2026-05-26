cmake_minimum_required(VERSION 3.30)
cmake_policy(VERSION 3.30)
project(kernunos VERSION 0.1.0 LANGUAGES C CXX Fortran)
set(CMAKE_MODULE_PATH cmake)
include(cmake/FortranEnvironment.cmake)
enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
# /opt/cuda/bin/nvcc is the compiler
#enable_language(ASM)

set (CMAKE_Fortran_COMPILER_ID "gfortran")
set (CMAKE_Fortran_COMPILER "gfortran")
set (CMAKE_C_COMPILER "gcc")
# these don't override system environment, unfortunately
set (FC "gfortran")
set (CXX "g++")
set (CC "gcc")

# toolchain-mingw64.cmake
#set(CMAKE_SYSTEM_NAME Windows)
#set(CMAKE_SYSTEM_PROCESSOR x86_64)
#set(CMAKE_CROSSCOMPILING true)
#set(CMAKE_SYSTEM_VERSION 10.0)

# specify the cross compiler
#set(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
#set(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
#set(CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)

# where is the target environment
#set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32)

# search for programs in the build host directories
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# for libraries and headers in the target directories
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

if(MINGW)
    message(STATUS "Building with MinGW")
else()
    message(STATUS "Not building with MinGW")
endif()




if(POLICY CMP0167)
cmake_policy(SET CMP0167 NEW)
endif(POLICY CMP0167)

set(QT_CREATOR_SKIP_MAINTENANCE_TOOL_PROVIDER on)
set(CMAKE_CXX_STANDARD 17) # doesn't apply to win32
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)
# should read again how to manage compiler flags with Cmake
if(CMAKE_Fortran_COMPILER MATCHES "gfortran")
#    set(dialect "-std=f2018 -cpp -ffree-line-length-none -fall-intrinsics")  #libmath77 multiple failures with deprecated features
     set(dialect "-std=gnu")
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
set(QT_CREATOR_SKIP_MAINTENANCE_TOOL_PROVIDER ON)
#SET(Qt5_DIR "/usr/lib/x86_64-linux-gnu/qt5")
SET(Qt6_DIR "/usr/lib/x86_64-linux-gnu/qt6")
#find_package(Qt5 REQUIRED COMPONENTS Core Gui Widgets OpenGL)
#find_package(Qt6 REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools)
find_package(QT NAMES Qt6 REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools)
find_package(Qt${QT_VERSION_MAJOR} REQUIRED COMPONENTS Core Gui Charts OpenGLWidgets OpenGL Widgets LinguistTools 3DCore 3DExtras Pdf PdfWidgets)
find_package(Qt${QT_VERSION_MAJOR} COMPONENTS Concurrent REQUIRED)

# Find the FreeType2 package
set(FREETYPE_DIR "/usr/include/freetype2")
find_package(Freetype REQUIRED)
  message(STATUS "Found Freetype in ${FREETYPE_INCLUDE_DIRS}")
  message(STATUS "Found Freetype in ${FREETYPE_DIR}")

set(TS_FILES src/cxx/kernunos_en_US.ts)


#GTK4
find_package (PkgConfig REQUIRED)
#pkg_check_modules(GTK4 REQUIRED gtk4)
#INCLUDE_DIRECTORIES(${GTK4_INCLUDE_DIRS})
#LINK_DIRECTORIES (${GTK4_LIBRARY_DIRS})
#GTK3
#pkg_check_modules(GTK3 REQUIRED gtk+-3.0)
#message(STATUS "----- GTK3_INCLUDE_DIRS: ${GTK3_INCLUDE_DIRS}")
#message(STATUS "----- GKT3_LIBRARIES: ${GTK3_LIBRARIES}")
#message(STATUS "----- GTK3_LINK_LIBRARIES: ${GTK3_LINK_LIBRARIES}")
#include_directories(${GTK3_INCLUDE_DIRS})
#link_directories(${GTK3_LIBRARY_DIRS})
#list(APPEND FC_DEP_LIBS ${GTK3_LIBRARIES})


find_library(GFORTRAN_LIBRARY NAMES gfortran)  # trying to fix ISO_Fortran_binding.h not found error, doesn't solve it, might be Intel compiler only
ADD_COMPILE_DEFINITIONS(ASSIMP_VERSION="${ASSIMP_VERSION}")
find_package(BLAS QUIET)
 if ( BLAS_FOUND )
  message(STATUS "BLAS found at ${BLAS_LIBRARIES}")
 else()
  set(BLAS_LIBRARIES "/usr/lib/libblas.so")
 endif()
find_package(LAPACK QUIET)
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
find_package(CUDAToolkit COMPONENTS cudart)
if(CUDAToolkit_FOUND)
    message("CUDA compiler: ${CUDAToolkit_NVCC_EXECUTABLE}")
    message("CUDA version: ${CUDAToolkit_VERSION}")
endif()
find_package(OpenMP)
#  boost, needed for gnuplot-iostream NOT system, edited also CmakeLists.txt in gnuplot-iostreams
find_package(Boost REQUIRED COMPONENTS iostreams filesystem)
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


#viennaCL
# ViennaCL example
#  include_directories(src/cxx/vienna/viennacl/*)
  #include(ViennaCLCommon)  #needs cmake/ViennaCLConfig.cmake, which might need path adjustments
#  add_executable(viennacl src/cxx/vienna/viennacl-info.cpp)
#  target_link_libraries(viennacl ${OPENCL_LIBRARIES} )
#  add_executable(sparse src/cxx/vienna/sparse.cpp)
#  target_link_libraries(sparse ${OPENCL_LIBRARIES} )
#  add_executable(blasexample src/cxx/vienna/blas3.cpp)
#  target_link_libraries(blasexample ${OPENCL_LIBRARIES} )

# needs to use nvcc
# Add a CUDA executable
#    add_executable(dense-blas-bench-cuda src/cxx/vienna/dense_blas.cu)
#    target_link_libraries(dense-blas-bench-cuda INTERFACE CUDAToolkit::cudart)

# Compile.

#   PLY conversion from  https://w3.impa.br/~diego/software/rply/
#                        https://github.com/diegonehab/rply
#   copy convert.c from rply/etc to rply and both to src/c/rply
#    file(COPY rply/etc/convert.c DESTINATION src/c/rply)
#    file(COPY rply/rply.c rply/rply.h rply/rplyfile.h DESTINATION src/c/rply)
#    file(GLOB_RECURSE src/c/rply *.c .h)
#    add_executable(ConvertPLYtoBIN src/c/rply/convert.c src/c/rply/rply.c )

#   C source 

add_executable(CleanSemicolons src/c/CleanSemicolons.c)

#   rply conversion 
    file(GLOB rply_sources src/c/convertplytobin.c src/c/rply.c src/c/*.h )
    file(GLOB interface src/c/logc.c src/c/logc.h
                 src/c/Ccounter.c src/c/counter.h)

#   Fortran Source

#    file(GLOB_RECURSE math77_lib     math77/src/MATH77/*.f)
    file(GLOB_RECURSE lib_sources90  src/src90/*.f90 src/src90/*.f90*.for)
    file(GLOB_RECURSE mod_sources90  src/mod90/*.f90
                                     src/HansonHopkins/set_precision.f90
                                     src/HansonHopkins/LapackInterface.f90)

#add_library( libmath77 ${math77_lib})

# external libraries
    set(LIBS ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${SUPERLU_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES} freetype ${GFORTRAN_LIBRARY})

# need this so that GLwidget.h can be found by AUTOUIC and ui.mainwindow.h, and for rply and assimp
include_directories(src/cxx src/c)

#superlu example from chapter12

set(LIBS2 ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${SUPERLU_LIBRARIES})
set(HH_SOURCES
                   src/HansonHopkins/driverPiecewiseLinear.f90
                   src/HansonHopkins/set_precision.f90
                   src/HansonHopkins/LapackInterface.f90
                   src/sparse/sparsekit.f90
                   src/sparse/sparsekit_test01.f90
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
                   src/src90/gauss-jordan.f90
                   )
               add_executable(driverPiecewiseLinear
                   ${HH_SOURCES}
               )
target_link_libraries(driverPiecewiseLinear ${LIBS2})

set(HH_SOURCES2
                   src/HansonHopkins/driverPiecewiseCubic.f90
                   src/HansonHopkins/set_precision.f90
                   src/HansonHopkins/LapackInterface.f90
                   src/sparse/sparsekit.f90
                   src/sparse/sparsekit_test01.f90
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
               add_executable(driverPiecewiseCubic
                   ${HH_SOURCES2}
               )
target_link_libraries(driverPiecewiseCubic ${LIBS2})

#kernunos Qt version

set(sparse_sources
                   src/sparse/sparsekit.f90
                   src/sparse/sparsekit_test01.f90
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

add_library( rply_includes ${rply_sources})
add_library( modules ${mod_sources90} ${interface})
#add_library( splia ${lib_sources90} ${math77_lib} ${interface})
add_library( splia ${lib_sources90} ${interface})

set(PROJECT_SOURCES
                   src/c/logc.c src/c/logc.h
                   src/c/get_compiler_name.h
                   src/c/charcount.c src/c/charcount.h
                   src/c/Ccounter.c src/c/counter.h
                   src/c/ConvertPLYtoBIN.c
                   src/c/CleanSemicolons_C.c
                   gnuplot-iostream/gnuplot-iostream.h
                   src/cxx/kernunos.cpp src/cxx/kernunos.h
                   src/cxx/GLwidget.cpp src/cxx/GLwidget.h
                   src/cxx/assistant.cpp src/cxx/assistant.h
 #                  src/cxx/shader_utils.cpp src/cxx/shader_utils.h
                   src/cxx/gnuplot_lioc.cpp
                   src/cxx/gnuplot_load.cpp
                   src/cxx/mainwindow.ui
                   src/src90/ConvertOFFtoSTL_C.f90
                   src/src90/get_compiler_name.f90
                   src/src90/janus.f90 
                   ${TS_FILES}
                   )

if(${QT_VERSION_MAJOR} GREATER_EQUAL 6)
    qt_add_executable(kernunos
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES} ${sparse_sources}
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
#    MACOSX_BUNDLE_GUI_IDENTIFIER my.example.com
#    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
#    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
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
    Boost::filesystem)

# Target Freetype
target_include_directories(kernunos PUBLIC FREETYPE_INCLUDE_DIRS)
target_include_directories(kernunos PUBLIC /usr/include/freetype2)

target_link_libraries(kernunos
               PRIVATE OpenMP::OpenMP_CXX Qt6::Concurrent
               gnuplot_iostream
               boost_iostreams
               boost_filesystem
               PUBLIC Qt${QT_VERSION_MAJOR}::Core
                      Qt${QT_VERSION_MAJOR}::OpenGLWidgets
                      Qt${QT_VERSION_MAJOR}::Gui
                      Qt${QT_VERSION_MAJOR}::Widgets
                      Qt${QT_VERSION_MAJOR}::OpenGL
                      Qt${QT_VERSION_MAJOR}::3DCore
                      Qt${QT_VERSION_MAJOR}::3DExtras
                      Qt${QT_VERSION_MAJOR}::Pdf
                      Qt${QT_VERSION_MAJOR}::PdfWidgets
                      assimp modules splia rply_includes ${LIBS}
           )


if(NOT DEFINED INSTALL_EXAMPLESDIR)
    set(INSTALL_EXAMPLESDIR "../")
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

