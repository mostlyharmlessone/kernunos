cmake_minimum_required(VERSION 3.30)
cmake_policy(VERSION 3.30)

#uncomment for Darwin
#set(CMAKE_Fortran_INCLUDES_FLAG "-I")
#set(CMAKE_FIND_FRAMEWORK LAST)
#project(kernunos LANGUAGES C CXX Fortran)
#set(CMAKE_Fortran_INCLUDE_FLAG "-I" CACHE STRING "Force -I instead of -F" FORCE)

set(VERSION_MAJOR "0")
set(VERSION_MINOR "2")
set(VERSION_BugFix "1")
set(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})

# need these for Darwin
set(PROJECT_VERSION  ${VERSION_MAJOR})
set(PROJECT_VERSION_MAJOR ${VERSION_MAJOR})
set(PROJECT_VERSION_MINOR  ${VERSION_MINOR})

#set(QT_FORCE_CMP0156_TO_VALUE OLD)
#set(CMAKE_MODULE_PATH cmake)

set(CMAKE_HOST_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM UNIX)

#set(CMAKE_HOST_SYSTEM_NAME Windows)
#set(CMAKE_SYSTEM Windows)

#set(CMAKE_HOST_SYSTEM_NAME Darwin)
#set(CMAKE_SYSTEM APPLE)

#set(CMAKE_HOST_SYSTEM_PROCESSOR arm64)
set(CMAKE_HOST_SYSTEM_PROCESSOR x86_64) #host
#set(CMAKE_SYSTEM_PROCESSOR x86_64) #target

#set(CMAKE_BUILD_TYPE RELEASE) # no debug or bounds check
set(CMAKE_BUILD_TYPE DEBUG) #this include debugging with addition of bounds flags as specified below

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  message(STATUS "Build host runs Linux")
 set (CMAKE_Fortran_COMPILER "gfortran")
 set(CMAKE_C_COMPILER "gcc")
 set(CMAKE_CXX_COMPILER "g++")
 # these don't override system environment, unfortunately
# set (FC "gfortran")
# set (CXX "g++")
# set (CC "gcc")
 set (FC ${CMAKE_Fortran_COMPILER})
 set (CXX  ${CMAKE_CXX_COMPILER})
 set (CC ${CMAKE_C_COMPILER})
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
 message(STATUS "Host system is Windows")
 set(CMAKE_RC_COMPILER "C:/Qt/Tools/mingw1310_64/bin/windres.exe")
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
 message(STATUS "Target and host system is Apple/Darwin")
 set(CMAKE_PREFIX_PATH "/usr/local/opt/llvm")
#these are for the iMac with M3
if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
# set (CMAKE_Fortran_COMPILER "/opt/homebrew/bin/gfortran-16")
# set(CMAKE_C_COMPILER "/opt/homebrew/bin/gcc-16")
# set(CMAKE_CXX_COMPILER "/opt/homebrew/bin/g++-16")
elseif(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "x86_64")
# fights the Qt IDE and results in an endless loop; remove if using QtCreator; have to set FC,CC,CXX environment vars
#export FC="/usr/local/bin/flang"
#export CC="/usr/local/bin/clang"
#export CXX="/usr/local/bin/clang++"
# set(CMAKE_Fortran_COMPILER "/usr/local/bin/gfortran-16")
# set(CMAKE_C_COMPILER "/usr/local/bin/gcc-16")
# set(CMAKE_CXX_COMPILER "/usr/local/bin/g++-16")
else()
 message(STATUS "Unknown architecture: ${CMAKE_HOST_SYSTEM_PROCESSOR}")
endif()
 # these don't override system environment, unfortunately
 set (FC ${CMAKE_Fortran_COMPILER})
 set (CXX  ${CMAKE_CXX_COMPILER})
 set (CC ${CMAKE_C_COMPILER})
 list(APPEND CMAKE_PREFIX_PATH " /usr/local/opt/include/freetype2")
 list(APPEND CMAKE_PREFIX_PATH " /usr/local/opt/freetype")
 list(APPEND CMAKE_PREFIX_PATH " /usr/local/opt/lapack")
# list(APPEND CMAKE_PREFIX_PATH "~/Qt/6.11.1/macos")
 list(APPEND CMAKE_PREFIX_PATH "/usr/local/bin")
# list(APPEND CMAKE_PREFIX_PATH "/usr/local/Cellar")
#  list(APPEND CMAKE_PREFIX_PATH "/usr/local/Cellar/gcc/16.2.0")
# list(APPEND CMAKE_PREFIX_PATH "/usr/local/Cellar/openblas/0.3.34")
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
else()
 project(kernunos LANGUAGES C CXX Fortran)
endif()

enable_language(Fortran)
enable_language(CXX)
enable_language(C)

# useful for checking if compilers support intraoperability
include(FortranCInterface)
FortranCInterface_VERIFY()

set(QT_CREATOR_SKIP_MAINTENANCE_TOOL_PROVIDER on)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)

# needed for qt
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)
set(CMAKE_AUTOUIC ON)

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
# toolchain-mingw64.cmake
  message(STATUS "Target system is Windows")
  set(CMAKE_SYSTEM_NAME Windows)
  set(CMAKE_SYSTEM_PROCESSOR x86_64)
  set(CMAKE_CROSSCOMPILING true)
  set(CMAKE_SYSTEM_VERSION 10.0)

# specify the cross compiler Windows builds
if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
 set(CMAKE_CROSSCOMPILING false)
# fights the Qt IDE and results in an endless loop; remove if using QtCreator
# set (CMAKE_Fortran_COMPILER "C:/Qt/Tools/mingw1310_64/bin/x86_64-w64-mingw32-gfortran.exe")
# set(CMAKE_C_COMPILER "C:/Qt/Tools/mingw1310_64/bin/x86_64-w64-mingw32-gcc.exe")
# set(CMAKE_CXX_COMPILER "C:/Qt/Tools/mingw1310_64/bin/x86_64-w64-mingw32-g++.exe")
# set(CMAKE_RC_COMPILER "C:/Qt/Tools/mingw1310_64/bin/windres.exe")
# set(CMAKE_Fortran_COMPILER "C:/Program Files (x86)/Intel/oneAPI/compiler/2026.0/bin/ifx.exe")
# set(CMAKE_Fortran_PREPROCESS_SOURCE "<CMAKE_Fortran_COMPILER> -fpp <DEFINES> <INCLUDES> <FLAGS> -E <SOURCE> > <PREPROCESSED_SOURCE>")
elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
  set(CMAKE_CROSSCOMPILING true)
  set (CMAKE_Fortran_COMPILER "x86_64-w64-mingw32-gfortran")
  set(CMAKE_C_COMPILER "x86_64-w64-mingw32-gcc")
  set(CMAKE_CXX_COMPILER "x86_64-w64-mingw32-g++")
  set(CMAKE_RC_COMPILER "x86_64-w64-mingw32-windres")
endif()
# make icon
# test.o generated by windres
if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  set(CMAKE_FIND_ROOT_PATH "C:/Qt/6.8.3/mingw_64")
  set(CMAKE_FIND_ROOT_PATH "/C/Qt/6.8.3/mingw_64")
  add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/kernunos_icon.o
  COMMAND C:/Qt/Tools/mingw1310_64/bin/windres.exe ${CMAKE_CURRENT_SOURCE_DIR}/kernunos_icon.rc -O coff -o kernunos_icon.o
)
elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux") # in Linux
  # x86_64-w64-mingw32-windres test.rc -O coff -o test.o
  set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32) # Standard MinGW sysroot directory
  add_custom_command(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/kernunos_icon.o
  COMMAND x86_64-w64-mingw32-windres ${CMAKE_CURRENT_SOURCE_DIR}/kernunos_icon.rc -O coff -o kernunos_icon.o
)
endif()

# STATIC stuff (Windows)
 if (CMAKE_SYSTEM STREQUAL "Windows")
  set(BUILD_FOR_WIN TRUE)
 else()
  set(BUILD_FOR_WIN FALSE)
 endif()

 option(STATIC_BUILD "Build a static binary." ${BUILD_FOR_WIN})

  if (STATIC_BUILD)
            # Force find commands to search only the target environment packages
#            set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32) # Standard MinGW sysroot directory
#            set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
#            set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
#            set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
#            set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
             # Enforce fully static compilation
            # -static: Links your internal and external third-party libraries statically.
            # -static-libgcc -static-libstdc++: Removes dependencies on libgcc_s_sjlj-1.dll and libstdc++-6.dll.
            set(CMAKE_EXE_LINKER_FLAGS "-static -static-libgcc -static-libstdc++" CACHE STRING "" FORCE)
            set(CMAKE_EXE_LINKER_FLAGS_DEBUG "-static -static-libgcc -static-libstdc++" CACHE STRING "" FORCE)
            set(CMAKE_EXE_LINKER_FLAGS_RELEASE "-static -static-libgcc -static-libstdc++" CACHE STRING "" FORCE)
            set(CMAKE_FIND_LIBRARY_SUFFIXES ".a" CONFIG)
            set(BUILD_SHARED_LIBS OFF)
            # include plugins into static build on windows
            # (we lack support for static on other platforms right now)
            set(QT_PLUGINS SvgIcon WindowsIntegration WindowsVistaStyle)
    endif()
 endif()

#dialect: 'f95’, ‘f2003’, ‘f2008’, ‘f2018’, ‘f2023’, ‘gnu’, or ‘legacy’.
 if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
#    set(CMAKE_Fortran_FLAGS_DEBUG "-cpp  -ffree-form -std=legacy -fno-optimize-sibling-calls -Wall  -Wextra -Wimplicit-interface  -fcheck=all -fbacktrace -fbounds-check -g -Xpreprocessor  -fopenmp -lomp -ffpe-trap=invalid,zero,overflow  -finit-real=snan -finit-integer=-99999999 -O3  -march=native  -ffast-math  -funroll-loops -fno-underscoring" )
    set(CMAKE_Fortran_FLAGS_DEBUG "-cpp -ffree-form -O3" )

 else()
    set(dialect "-cpp -ffree-form -std=legacy -fno-optimize-sibling-calls") #gfortran/lapack/blas bug? -fexternal-blas
    set(bounds "-Wall -Wextra -Wimplicit-interface -fcheck=all -fbacktrace -fbounds-check -g")
    set(omp "-fopenmp")
    set(NaN "-ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-99999999")
#  profiling with gprof 
    set(profile "-pg -g -coverage")
    set(optimize "-O3 -march=native -ffast-math -funroll-loops")    

    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${dialect}")
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${optimize}")
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} ${omp}")

    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${dialect}")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${omp}")
   #set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${profile}")   #doesn't work for H&H possibly not for superlu
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${NaN}")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${type}")
 endif()

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 0) #set to 1 to show dependencies

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
set (CMAKE_PREFIX_PATH "C:/Qt/6.8.3/mingw_64")
list(APPEND CMAKE_PREFIX_PATH "C:/boost")
list(APPEND CMAKE_PREFIX_PATH "C:/boost/stage/lib/cmake")
list(APPEND CMAKE_PREFIX_PATH "C:/Program Files (x86)/OpenBLAS")
list(APPEND CMAKE_PREFIX_PATH "C:/Program Files (x86)/SuperLU")
list(APPEND CMAKE_PREFIX_PATH "C:/Program Files (x86)/Assimp")
list(APPEND CMAKE_PREFIX_PATH "C:/Program Files (x86)/freetype")
list(APPEND CMAKE_PREFIX_PATH "C:/Program Files (x86)/glm")
else()
 set (CMAKE_PREFIX_PATH "/usr/lib/")
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
  list(APPEND CMAKE_PREFIX_PATH "../Qt/6.8.3/mingw64/lib/cmake/")
 else()
 if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
#  set(Qt6_DIR "../Qt/6.11.1/macos/lib/cmake/Qt6/")
#  list(APPEND CMAKE_PREFIX_PATH "../Qt/6.11.1/macos/lib/cmake/Qt6/")
#  message(STATUS "Qt6 dir macOS: " ${Qt6_DIR})
 else()
  if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
#   list(APPEND CMAKE_PREFIX_PATH "../Qt/6.8.3/gcc_64/lib/cmake/")
  endif()
 endif()
endif()

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

if (${CMAKE_SYSTEM} MATCHES "Windows")
 if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows") #compiling for Windows under Windows
    find_package(BLAS REQUIRED)
    message("BLAS found at " ${BLAS_LIBRARIES})
    find_package(LAPACK REQUIRED)
    message(STATUS "LAPACK found at ${LAPACK_LIBRARIES}")
    find_package(OpenMP)
    if (OpenMP_FOUND)
     message(STATUS "OpenMP_CXX found at ${OpenMP_CXX_LIBRARIES}")
     message(STATUS "OpenMP_C found at ${OpenMP_C_LIBRARIES}")
     message(STATUS "OpenMP_Fortran found at ${OpenMP_Fortran_LIBRARIES}")
     message(STATUS "OpenMP_CXX_FLAGS found at ${OpenMP_CXX_FLAGS}")
     message(STATUS "OpenMP_C_FLAGS found at ${OpenMP_C_FLAGS}")
     message(STATUS "OpenMP_Fortran_FLAGS found at ${OpenMP_Fortran_FLAGS}")
    else()
#    set(OpenMP_LIBRARIES "location of openmp if not found under Windows")
    endif()
  else()  #cross-compiling for Windows under Linux
   set(BLAS_LIBRARIES "/usr/x86_64-w64-mingw32/lib/libblas.a")
   set(BLAS_FOUND TRUE)
   set(LAPACK_LIBRARIES "/usr/x86_64-w64-mingw32/lib/liblapack.a")
   set(LAPACK_FOUND TRUE)
   set(OpenMP_LIBRARIES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a")
   set(OpenMP_Fortran_FLAGS "-fopenmp")
   set(OpenMP_Fortran_LIB_NAMES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a")
   set(OpenMP_Fortran_FOUND TRUE)
 # find_package(OpenMP)
   set(OpenMP_C_FLAGS "-fopenmp")
   set(OpenMP_C_LIB_NAMES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a")
   set(OpenMP_C_LIBRARIES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a")
   set(OpenMP_C_FOUND TRUE)
   set(OpenMP_CXX_FLAGS "-fopenmp")
   set(OpenMP_CXX_LIB_NAMES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a;/usr/x86_64-w64-mingw32/lib/libmingwthrd.a")
   set(OpenMP_CXX_LIBRARIES "/usr/x86_64-w64-mingw32/lib/libgomp.a;/usr/x86_64-w64-mingw32/lib/libpthread.a;/usr/x86_64-w64-mingw32/lib/libmingwthrd.a")
   set(OpenMP_CXX_FOUND TRUE)
  endif()
 else() #compiling for Linux or Apple
  find_package(BLAS REQUIRED)
  if (BLAS_FOUND)
   message(STATUS "BLAS found at ${BLAS_LIBRARIES}")
  endif()
  find_package(LAPACK REQUIRED)
  if (LAPACK_FOUND)
   message(STATUS "LAPACK found at ${LAPACK_LIBRARIES}")
  endif()
  message(STATUS "Finding OpenMP under Linux or Apple: ")
  find_package(OpenMP)
  if (OpenMP_FOUND)
   message(STATUS "OpenMP_CXX found at ${OpenMP_CXX_LIBRARIES}")
   message(STATUS "OpenMP_C found at ${OpenMP_C_LIBRARIES}")
   message(STATUS "OpenMP_Fortran found at ${OpenMP_Fortran_LIBRARIES}")
   message(STATUS "OpenMP_CXX_FLAGS found at ${OpenMP_CXX_FLAGS}")
   message(STATUS "OpenMP_C_FLAGS found at ${OpenMP_C_FLAGS}")
   message(STATUS "OpenMP_Fortran_FLAGS found at ${OpenMP_Fortran_FLAGS}")
  endif()
endif()

# Qt

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
 set(QT_DIR "C:/Qt/6.8.3/mingw_64")
 find_package(Qt6 REQUIRED COMPONENTS Core Gui Widgets Concurrent Pdf PdfWidgets OpenGLWidgets OpenGL LinguistTools)
 message(STATUS "Qt Windows " ${Qt6_FOUND})
 # Find the glm package
 find_package(GLM REQUIRED)
# Find the FreeType2 package
# had to add the freetype project here, not just the things in C:/Program Files (x86)
 add_subdirectory(freetype/)
 find_package(Freetype REQUIRED)
 set(FREETYPE_DIR "C:/Program Files (x86)/freetype")
# assimp
 add_subdirectory(assimp/)
 find_package(assimp REQUIRED)
 message(STATUS "ASSIMP_INCLUDE_DIRS" ${ASSIMP_INCLUDE_DIRS})
# had to set these in Qt/Projects cmake settings under Windows "-DASSIMP_WARNINGS_AS_ERRORS=OFF"
 set(${ASSIMP_INCLUDE_DIRS} assimp/include/assimp)
 set(${ASSIMP_WARNINGS_AS_ERRORS}=OFF)
 set(${ASSIMP_BUILD_TESTS}=OFF)
else() #Linux or Apple host
  #also set in QtCreator
  set(QT_CREATOR_SKIP_MAINTENANCE_TOOL_PROVIDER ON)
  if (CMAKE_SYSTEM STREQUAL "Windows")
   find_package(Qt6 COMPONENTS Core Gui OpenGLWidgets OpenGL Widgets LinguistTools REQUIRED ) #Pdf PdfWidgets
   find_package(Qt6 COMPONENTS Concurrent REQUIRED)
   message(STATUS "Qt linux, cross compilation for Windows: " ${Qt6_FOUND})
   message(STATUS "Qt6 dir: " ${Qt6_DIR})
  else()
#  Linux or Apple no cross-compile
   find_package(Qt6 COMPONENTS Core Gui Pdf PdfWidgets OpenGLWidgets OpenGL Widgets LinguistTools REQUIRED )
   find_package(Qt6 COMPONENTS Concurrent REQUIRED)
   message(STATUS "Qt linux/apple, no crosscompilation: " ${Qt6_FOUND})
   message(STATUS "Qt6 dir: " ${Qt6_DIR})
    # Find the glm package
 find_package(GLM REQUIRED)
  endif()
# Find the FreeType2 package
  if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
#  find_package(PkgConfig REQUIRED)
#  pkg_check_modules(Freetype REQUIRED IMPORTED_TARGET freetype2)
#  set(${FREETYPE_INCLUDE_DIRS} "/usr/local/include/freetype2")
#  set(${FREETYPE_LIBRARIES} "/usr/local/lib")
    find_package(Freetype MODULE REQUIRED)
    message(STATUS "Found Freetype in ${FREETYPE_INCLUDE_DIRS}")
    message(STATUS "Found Freetype in ${FREETYPE_LIBRARIES}")
    add_library(freetype ALIAS Freetype::Freetype)
  else()
    find_package(PkgConfig REQUIRED)
    pkg_check_modules(Freetype REQUIRED IMPORTED_TARGET freetype2)
    set(${FREETYPE_INCLUDE_DIRS} "/usr/include/freetype2")
    set(${FREETYPE_LIBRARIES} "/usr/lib")
#    find_package(Freetype REQUIRED) # finds mingw version over linux version
#    message(STATUS "Found Freetype in ${FREETYPE_INCLUDE_DIRS}")
#    message(STATUS "Found Freetype in ${FREETYPE_DIR}")
   endif() 
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
# assimp
#uncomment below to build assimp
add_subdirectory(assimp/)
find_package(assimp REQUIRED)
message(STATUS "ASSIMP_INCLUDE_DIRS" ${ASSIMP_INCLUDE_DIRS})
message(STATUS "ASSIMP_LIBRARIES" ${ASSIMP_LIBRARIES})
# had to set these in Qt/Projects cmake settings
set(${ASSIMP_INCLUDE_DIRS} assimp/include/assimp)
set(${ASSIMP_WARNINGS_AS_ERRORS}=OFF)
set(${ASSIMP_BUILD_TESTS}=OFF)
set(${ASSIMP_INSTALL}=OFF)
#these are for the iMac with M3
 if(CMAKE_HOST_SYSTEM_PROCESSOR STREQUAL "arm64")
  set(${ASSIMP_INCLUDE_DIRS} /opt/homebrew/include/assimp)
  set(${ASSIMP_LIBRARIES} /opt/homebrew/lib)
 else()
  set(${ASSIMP_INCLUDE_DIRS} /usr/local/include/assimp)
  set(${ASSIMP_LIBRARIES} /usr/local/lib)
 endif()
endif()

 set(TS_FILES src/cxx/kernunos_en_US.ts)
 ADD_COMPILE_DEFINITIONS(ASSIMP_VERSION="${ASSIMP_VERSION}")

# superlu
 find_package(SuperLU REQUIRED)

# Compile.

#   C source

#    Free standing executable utilities

#    file(COPY rply/etc/convert.c DESTINATION src/c/rply)
#    file(COPY rply/rply.c rply/rply.h rply/rplyfile.h DESTINATION src/c/rply)
#    file(GLOB_RECURSE src/c/rply *.c .h)
#    add_executable(ConvertPLYtoBIN src/c/rply/convert.c src/c/rply/rply.c )
#    add_executable(CleanSemicolons src/c/CleanSemicolons.c)
#    add_executable(sparctxt src/c/sparctxt.c)
#    add_executable(dostxt src/c/dostxt.c)

#   rply conversion 
    file(GLOB rply_sources src/c/convertplytobin.c src/c/rply.c src/c/*.h )
 #   file(GLOB interface src/c/logc.c src/c/logc.h
 #                src/c/Ccounter.c src/c/counter.h)

#   C source

#   Fortran Source

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


# external libraries
    set(LIBS ${SUPERLU_LIBRARIES} ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${OPENGL_LIBRARIES} freetype ${GFORTRAN_LIBRARY})

# need this so that GLwidget.h can be found by AUTOUIC and ui.mainwindow.h, and for rply and assimp
include_directories(src/cxx src/c)

add_library(rply_includes ${rply_sources})

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
 # put everything below; module build seems broken on Darwin
 set( INSTALL_EXAMPLEDIR  macOS )
endif()

set(LIBS2 ${SUPERLU_LIBRARIES} ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} )

if(FALSE)

#superlu example from chapter12

set(HH_SOURCES
                   src/HansonHopkins/driverPiecewiseLinear.f90
                   src/HansonHopkins/set_precision.f90
                   src/HansonHopkins/LapackInterface.f90
                   src/src90/gauss-jordan.f90
                   )

qt_add_executable(driverPiecewiseLinear
MANUAL_FINALIZATION
${HH_SOURCES} ${sparse_sources}
)

set_target_properties(driverPiecewiseLinear PROPERTIES
MACOSX_BUNDLE_GUI_IDENTIFIER driverPiecewiseLinear.qt
MACOSX_BUNDLE_DESTINATION ${INSTALL_EXAMPLEDIR}
MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
MACOSX_BUNDLE TRUE
WIN32_EXECUTABLE TRUE
)

target_link_libraries(driverPiecewiseLinear PUBLIC ${LIBS2})

install(TARGETS driverPiecewiseLinear RUNTIME DESTINATION bin 
                BUNDLE DESTINATION macOS 
                LIBRARY DESTINATION lib)

if(QT_VERSION_MAJOR EQUAL 6)
    qt_finalize_executable(driverPiecewiseLinear)
endif()


set(HH_SOURCES2
                   src/HansonHopkins/driverPiecewiseCubic.f90
                   src/HansonHopkins/set_precision.f90
                   src/HansonHopkins/LapackInterface.f90
                   )
               add_executable(driverPiecewiseCubic
                   ${HH_SOURCES2} ${sparse_sources}
               )
target_link_libraries(driverPiecewiseCubic ${LIBS2})

endif()

#kernunos Qt version

 set(PROJECT_SOURCES
        src/HansonHopkins/set_precision.f90
        src/HansonHopkins/LapackInterface.f90
        src/src90/gauss-jordan.f90
        src/src90/AdjustRadSplineCenter.f90
        src/src90/Pupil.f90
        src/src90/WriteGeomPLY.f90
        src/src90/makelegend.f90
        src/src90/ConvertOFFtoSTL_C.f90
        src/src90/SplineCenter.f90
        src/src90/bsearch.f90
        src/src90/nspline.f90
        src/src90/CubicSplineQuad.f90
        src/src90/SplineEval.f90
        src/src90/dctsv.f90
        src/src90/nsplineCenter.f90
        src/src90/Geom.f90
        src/src90/SplineEval1Dx1D.f90
        src/src90/printgraph.f90
        src/src90/LSQeval.f90
        src/src90/SplineEvalCenter.f90
        src/src90/get_compiler_name.f90
        src/src90/pspli.f90
        src/src90/LSQfit.f90
        src/src90/WriteCenter.f90
        src/src90/rcnvrtT.f90
        src/src90/LSQspline.f90
        src/src90/WriteCenterJ.f90
        src/src90/linSplineEval.f90
        src/src90/thomas.f90
        src/src90/MakeRadSplineCenter.f90
        src/src90/WriteGeomOFF.f90
        src/src90/linearspline.f90
        src/src90/trapez.f90
        src/mod90/M_color.f90
        src/mod90/cornea_arrays.f90
        src/mod90/parameters.f90
        src/mod90/spline_interfaces.f90
        src/mod90/c_interfaces.f90
        src/mod90/io_functions.f90
        src/mod90/special_fct.f90
        src/c/logc.c src/c/logc.h
        src/c/get_compiler_name.h
        src/c/charcount.c src/c/charcount.h
        src/c/Ccounter.c src/c/counter.h
        src/c/ConvertPLYtoBIN.c
        src/c/CleanSemicolons_C.c
        src/cxx/kernunos.cpp src/cxx/kernunos.h
        src/cxx/GLwidget.cpp src/cxx/GLwidget.h
        src/cxx/assistant.cpp src/cxx/assistant.h
        src/cxx/mainwindow.ui
        src/src90/janus.f90
         ${TS_FILES}
)

if (TRUE)

#Qt6 or greater

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
    qt_add_executable(kernunos
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES} ${sparse_sources} resources/kernunos.icns
  )
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
    qt_add_executable(kernunos
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES} ${sparse_sources} kernunos_icon.o
    #can try kernunos.rc and no custom command
  )
endif()

if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    qt_add_executable(kernunos
    MANUAL_FINALIZATION
    ${PROJECT_SOURCES} ${sparse_sources}
  )
endif()

    qt_create_translation(QM_FILES ${CMAKE_SOURCE_DIR} ${TS_FILES})

set(${KMP_DUPLICATE_LIB_OK}=TRUE)

set_target_properties(kernunos PROPERTIES
    MACOSX_BUNDLE_GUI_IDENTIFIER kernunos.qt
    MACOSX_BUNDLE_DESTINATION ${INSTALL_EXAMPLEDIR}
    MACOSX_BUNDLE_ICON_FILE kernunos.icns
    MACOSX_PACKAGE_LOCATION "Resources"
    MACOSX_BUNDLE_BUNDLE_VERSION ${PROJECT_VERSION}
    MACOSX_BUNDLE_SHORT_VERSION_STRING ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    MACOSX_BUNDLE TRUE
    WIN32_EXECUTABLE TRUE
)

set_source_files_properties(resources/kernunos.icns PROPERTIES MACOSX_PACKAGE_LOCATION "Resources")

# Target Freetype
if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
 target_include_directories(kernunos PUBLIC ${FREETYPE_INCLUDE_DIRS})
 target_include_directories(kernunos PUBLIC ${FREETYPE_DIR})
else()
if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
  target_include_directories(kernunos PUBLIC ${FREETYPE_INCLUDE_DIRS})
  target_include_directories(kernunos PUBLIC /usr/local/include/freetype2)
else()
 target_include_directories(kernunos PUBLIC ${FREETYPE_INCLUDE_DIRS})
 target_include_directories(kernunos PUBLIC /usr/include/freetype2)
 endif()
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
 target_link_libraries(kernunos
                PRIVATE
                Qt6::Concurrent
                OpenMP::OpenMP_CXX
                PUBLIC Qt${QT_VERSION_MAJOR}::Core
                Qt${QT_VERSION_MAJOR}::OpenGLWidgets
                Qt${QT_VERSION_MAJOR}::Gui
                Qt${QT_VERSION_MAJOR}::Widgets
                Qt${QT_VERSION_MAJOR}::OpenGL
                Qt${QT_VERSION_MAJOR}::Pdf
                Qt${QT_VERSION_MAJOR}::PdfWidgets
                assimp rply_includes ${LIBS}
# need the OpenGl library from e.g. C:\Program Files (x86)\Windows Kits\10\Lib\10.0.26100.0\um\x64
                OpenGL32.Lib
            )
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
 target_link_libraries(kernunos
                PRIVATE
                Qt6::Concurrent
                OpenMP::OpenMP_CXX
                PUBLIC Qt${QT_VERSION_MAJOR}::Core
                Qt${QT_VERSION_MAJOR}::OpenGLWidgets
                Qt${QT_VERSION_MAJOR}::Gui
                Qt${QT_VERSION_MAJOR}::Widgets
                Qt${QT_VERSION_MAJOR}::OpenGL
                Qt${QT_VERSION_MAJOR}::Pdf
                Qt${QT_VERSION_MAJOR}::PdfWidgets
                assimp rply_includes ${LIBS}
           )
endif()

if (CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
 target_link_libraries(kernunos
                PRIVATE
                Qt${QT_VERSION_MAJOR}::Concurrent
                OpenMP::OpenMP_CXX # if (CMAKE_SYSTEM MATCHES Windows)
                PUBLIC Qt${QT_VERSION_MAJOR}::Core
                Qt${QT_VERSION_MAJOR}::OpenGLWidgets
                Qt${QT_VERSION_MAJOR}::Gui
                Qt${QT_VERSION_MAJOR}::Widgets
                Qt${QT_VERSION_MAJOR}::OpenGL
# these work in Qt but not outside it in a terminal cmake
                Qt${QT_VERSION_MAJOR}::Pdf         # if (CMAKE_SYSTEM MATCHES Windows)
                Qt${QT_VERSION_MAJOR}::PdfWidgets  # if (CMAKE_SYSTEM MATCHES Windows)
                assimp rply_includes ${LIBS}
           )
endif()

install(TARGETS kernunos
                RUNTIME DESTINATION bin
                BUNDLE DESTINATION macOS
                LIBRARY DESTINATION lib
                )

if(QT_VERSION_MAJOR EQUAL 6)
    qt_finalize_executable(kernunos)
endif()

endif() #temp if block

# Under Windows after compilationrun: /c/Qt/6.8.3/mingw_64/bin/windeployqt6.exe kernunos.exe
