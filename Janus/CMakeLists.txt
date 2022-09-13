cmake_minimum_required(VERSION 3.16)
cmake_policy(VERSION 3.16)

project(Jupiter VERSION 0.1.0 LANGUAGES C CXX Fortran)

enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
# /opt/cuda/bin/nvcc is the compiler
#enable_language(ASM)

set (CMAKE_Fortran_COMPILER_ID "gfortran")
set (FC "gfortran") 
set(CMAKE_CXX_STANDARD 17) # doesn't apply to win32
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)
# should read again how to manage compiler flags with Cmake
if(CMAKE_Fortran_COMPILER_ID MATCHES "gfortran")
    set(dialect "-ffree-form -std=f2008")
#     set(dialect "-std=gnu")
     set(bounds "-fbounds-check")
     set(omp "-fopenmp")
     set(NaN "-Wall -Wextra -Wimplicit-interface -g -fcheck=all -fbacktrace -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-99999999")
#  profiling with gprof 
 #     set(-pg)    
#  src profiling with gcov -a -b xxx.f90
#     set(-g -coverage) 
    set(optimize "-O3 -march=native -ffast-math -funroll-loops")    
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand f08 -free -implicitnone")
    set(bounds "-check bounds")
endif()
#  nvfortran==pgi
if(CMAKE_Fortran_COMPILER_ID MATCHES "pgfortran")
#    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
#    set(bounds "-C")
endif()

# needed for qt
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)
if(CMAKE_VERSION VERSION_LESS "3.7.0")
    set(CMAKE_INCLUDE_CURRENT_DIR ON)
endif()

# set(CMAKE_BUILD_TYPE Release) # no debug or bounds check
set(CMAKE_BUILD_TYPE Debug) #this include debugging with addition of bounds flags as specified below
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
#set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${optimize}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${omp}")
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${NaN}")

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 0) #set to 1 to show dependencies

set (CMAKE_PREFIX_PATH "/usr/local")

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

if(WIN32)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
endif(WIN32)

link_directories(${CMAKE_SOURCE_DIR}/lib)

# find the required packages
find_package(GLM REQUIRED)
message(STATUS "Found GLM ")
find_package(GLFW3 REQUIRED)
message(STATUS "Found GLFW3 ")
set(EPOXY_ROOT_DIR ${LIBDIR}/libepoxy)
find_package(LibEpoxy REQUIRED)
include_directories(${LibEpoxy_INCLUDE_DIRS})
 message(STATUS "Found LibEpoxy in ${LIBEPOXY_LIBRARIES}")
if(UNIX AND NOT APPLE)
 set(wxWidgets_CONFIG_OPTIONS_DEFAULT "--toolkit=gtk3")
endif()
#find_package(wxWidgets 3 REQUIRED gl richtext aui adv html webview core xml net base)
find_package(wxWidgets 3 REQUIRED gl core net base)
if(wxWidgets_FOUND)
  include(${wxWidgets_USE_FILE})
      message("-- FOUND WxWidgets library")
else()
    message("Failed to find WxWidgets library")
endif()
# Qt
find_package(Qt5 REQUIRED COMPONENTS Core Gui Widgets OpenGL)
#GTK4
find_package (PkgConfig REQUIRED)
pkg_check_modules(GTK4 REQUIRED gtk4)
INCLUDE_DIRECTORIES(${GTK4_INCLUDE_DIRS})
LINK_DIRECTORIES (${GTK4_LIBRARY_DIRS})
pkg_check_modules(GTK3 REQUIRED gtk+-3.0)
message(STATUS "----- GTK3_INCLUDE_DIRS: ${GTK3_INCLUDE_DIRS}")
message(STATUS "----- GKT3_LIBRARIES: ${GTK3_LIBRARIES}")
message(STATUS "----- GTK3_LINK_LIBRARIES: ${GTK3_LINK_LIBRARIES}")
include_directories(${GTK3_INCLUDE_DIRS})
link_directories(${GTK3_LIBRARY_DIRS})
list(APPEND FC_DEP_LIBS ${GTK3_LIBRARIES})
find_package(BLAS)
 if ( BLAS_FOUND )
  message(STATUS "BLAS found at ${BLAS_LIBRARIES}")
 else()
  set(BLAS_LIBRARIES "/usr/local/lib/libblas.so")
 endif()
find_package(LAPACK)
 if ( LAPACK_FOUND )
  message(STATUS "LAPACK found at ${LAPACK_LIBRARIES}")
 else()
  set(LAPACK_LIBRARIES "/usr/lib/i386-linux-gnu/lapack/liblapack.so.3")
 endif()
find_package(SuperLU)
 if ( SUPERLU_FOUND )
  message(STATUS "SUPERLU found at ${SUPERLU_LIBRARIES}")   
 else()
  set(SUPERLU_LIBRARIES "/usr/local/lib/libsuperlu.so")
 endif()
find_package(OpenCL)
 if ( OPENCL_FOUND )
  message(STATUS "OPENCL found at ${OPENCL_LIBRARIES}")   
 else()
  set(OPENCL_LIBRARIES "/usr/lib/libOpenCL.so")
 endif()
find_package(OpenMP)
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
  add_executable(viennacl src/cxx/vienna/viennacl-info.cpp)  
  target_link_libraries(viennacl ${OPENCL_LIBRARIES} ) 
  add_executable(sparse src/cxx/vienna/sparse.cpp)  
  target_link_libraries(sparse ${OPENCL_LIBRARIES} )
  add_executable(blasexample src/cxx/vienna/blas3.cpp)  
  target_link_libraries(blasexample ${OPENCL_LIBRARIES} )

# Compile.

#   PLY conversion from  https://w3.impa.br/~diego/software/rply/
#                        https://github.com/diegonehab/rply
#   copy convert.c from rply/etc to rply and both to src/c/rply
    file(COPY rply/etc/convert.c DESTINATION src/c/rply)
    file(COPY rply/rply.c rply/rply.h rply/rplyfile.h DESTINATION src/c/rply)
    file(GLOB_RECURSE src/c/rply *.c .h)
    add_executable(ConvertPLYtoBIN src/c/rply/convert.c src/c/rply/rply.c )  
#   C source 
#   rply conversion 
    file(GLOB rply_sources  src/c/rply/convertplytobin.c src/c/rply/rply.c src/c/rply *.h )
#   gtk example with gtk4 & blas fortran
    file(GLOB blas_f2c src/c/gtk/c_dgemm.f90)
    add_library( blas_f2c ${blas_f2c})
#    add_executable(glarea src/c/gtk/glarea-app-window.c )
    add_executable(example-2 src/c/gtk/gtkexample2.c)
#    target_link_libraries(glarea ${GTK3_LIBRARIES} epoxy)
    target_link_libraries(example-2 ${GTK4_LIBRARIES} blas_f2c ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
#   C++ source
    file(GLOB cpp_src src/cxx/opengl_show.cpp *.h src/cxx/mathstuff.cpp)
#   qt
##    add_executable(qt-example src/cxx/qt/MyOpenGL/main.cpp src/cxx/qt/MyOpenGL/window.cpp src/cxx/qt/MyOpenGL/myglwidget.cpp ${UI_HEADERS})
##    target_link_libraries(qt-example Qt5::Core Qt5::OpenGL Qt5::Gui ${OPENGL_LIBRARIES})
#   Fortran Source
    file(GLOB_RECURSE lib_sources90  src/lib90 *.f90 *.for)
    file(GLOB_RECURSE mod_sources90  src/modules *.f90)
#   
    add_library( rply_includes ${rply_sources})
    add_library( cpp_includes ${cpp_src})  
    add_library( modules ${mod_sources90})
    add_library( splia ${lib_sources90})

    set(PROJECT_LIBS splia modules rply_includes cpp_includes )
    set(OUTSIDE_LIBS ${wxWidgets_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES} glfw )   
    set(LIBS ${PROJECT_LIBS} ${OUTSIDE_LIBS})

    add_executable(${PROJECT_NAME} src/cxx/Jupiter.cpp src/lib90/janus.f90 )
    add_executable(juno src/lib90/juno.f90 src/lib90/janus.f90)

    add_executable(pyramid src/cxx/pyramid/pyramid.cpp src/cxx/pyramid/oglstuff.cpp src/cxx/pyramid/mathstuff.cpp)
    target_link_libraries(pyramid ${wxWidgets_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES})

    
    target_link_libraries(${PROJECT_NAME} PRIVATE OpenMP::OpenMP_CXX PUBLIC splia modules rply_includes cpp_includes ${wxWidgets_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES} glfw)
    
    target_link_libraries(juno PRIVATE OpenMP::OpenMP_CXX PUBLIC splia modules rply_includes cpp_includes ${wxWidgets_LIBRARIES} ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OPENGL_LIBRARIES} ${LIBEPOXY_LIBRARIES} glfw)
        
    #target_link_libraries(${PROJECT_NAME} PRIVATE OpenMP::OpenMP_CXX PUBLIC ${LIBS})     




