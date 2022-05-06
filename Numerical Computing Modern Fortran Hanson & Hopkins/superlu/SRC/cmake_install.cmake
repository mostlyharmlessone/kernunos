# Install script for directory: /home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "DEBUG")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so.5.2.1"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so.5"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      file(RPATH_CHECK
           FILE "${file}"
           RPATH "/usr/local/lib")
    endif()
  endforeach()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/libsuperlu.so.5.2.1"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/libsuperlu.so.5"
    )
  foreach(file
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so.5.2.1"
      "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so.5"
      )
    if(EXISTS "${file}" AND
       NOT IS_SYMLINK "${file}")
      file(RPATH_CHANGE
           FILE "${file}"
           OLD_RPATH "::::::::::::::"
           NEW_RPATH "/usr/local/lib")
      if(CMAKE_INSTALL_DO_STRIP)
        execute_process(COMMAND "/usr/bin/strip" "${file}")
      endif()
    endif()
  endforeach()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so"
         RPATH "/usr/local/lib")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE SHARED_LIBRARY FILES "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/libsuperlu.so")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so"
         OLD_RPATH "::::::::::::::"
         NEW_RPATH "/usr/local/lib")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/libsuperlu.so")
    endif()
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/supermatrix.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_Cnames.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_dcomplex.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_scomplex.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_util.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/superlu_enum_consts.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_sdefs.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_ddefs.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_cdefs.h"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/slu_zdefs.h"
    )
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu/superluTargets.cmake")
    file(DIFFERENT EXPORT_FILE_CHANGED FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu/superluTargets.cmake"
         "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/CMakeFiles/Export/lib/cmake/superlu/superluTargets.cmake")
    if(EXPORT_FILE_CHANGED)
      file(GLOB OLD_CONFIG_FILES "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu/superluTargets-*.cmake")
      if(OLD_CONFIG_FILES)
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu/superluTargets.cmake\" will be replaced.  Removing files [${OLD_CONFIG_FILES}].")
        file(REMOVE ${OLD_CONFIG_FILES})
      endif()
    endif()
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu" TYPE FILE FILES "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/CMakeFiles/Export/lib/cmake/superlu/superluTargets.cmake")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Dd][Ee][Bb][Uu][Gg])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu" TYPE FILE FILES "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/CMakeFiles/Export/lib/cmake/superlu/superluTargets-debug.cmake")
  endif()
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/cmake/superlu" TYPE FILE FILES
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/superluConfig.cmake"
    "/home/debeus/Janus/Numerical Computing Modern Fortran Hanson & Hopkins/superlu/SRC/superluConfigVersion.cmake"
    )
endif()

