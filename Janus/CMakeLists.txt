cmake_minimum_required(VERSION 3.16)
project(Janus)

enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
enable_language(ASM)

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# Libigl
 option(LIBIGL_GLFW "Build target igl::glfw" ON)
# option (LIBIGL_WITH_OPENGL "Use OpenGL" ON)
# option (LIBIGL_WITH_OPENGL_GLFW "Use GLFW" ON)
# option (LIBIGL_WITH_OPENGL_GLFW_IMGUI "Use ImGui" ON)
 include(libigl)

# Add your project files
file(GLOB SRC_FILES_2 src/cxx/view2/*.cpp)
add_executable(view2 ${SRC_FILES_2})
target_link_libraries(view2 PUBLIC igl::glfw)

file(GLOB SRC_FILES_1 src/cxx/view1/*.cpp)
add_executable(view1 ${SRC_FILES_1})
target_link_libraries(view1 PUBLIC igl::glfw)

#file(GLOB SRC_FILES src/cxx/view/*.cpp src/cxx/view/*.h)
#add_executable(view ${SRC_FILES})
#target_link_libraries(view PUBLIC igl::glfw)

#    file(GLOB_RECURSE src/cxx *.cpp *.h)
#    add_executable(view2 src/cxx/main.cpp)
#    target_include_directories(view2 PRIVATE "${GLAD_DIR}/include")    
 #   target_link_libraries(view2 igl::core igl::opengl_glfw "glad")
 #   add_executable(view1 src/cxx/view1.cpp)
#    target_include_directories(view2 PRIVATE "${GLAD_DIR}/include")    
#    target_link_libraries(view1 igl::core igl::opengl_glfw "glad")
#    add_executable(view src/cxx/view.cpp)
 #   target_include_directories(view PRIVATE "${GLAD_DIR}/include")    
#    target_link_libraries(view igl::core igl::opengl_glfw "glad" ${wxWidgets_LIBRARIES})   

set (CMAKE_Fortran_COMPILER_ID "gfortran")

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 1) #second option shows dependencies

# after wiping CMakeFiles folder run the following to reset

set (CMAKE_PREFIX_PATH "/usr/local")

set (FC "gfortran") 
#cmake -DCMAKE_Fortran_COMPILER_ID=gfortran ./
if(CMAKE_Fortran_COMPILER_ID MATCHES "gfortran")
#    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
     set(dialect "-std=gnu")
     set(bounds "-fbounds-check")
     set(omp "-fopenmp")
#  profiling with gprof 
 #     set(-pg)    
#  src profiling with gcov -a -b xxx.f90
#     set(-g -coverage)     
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

#wxWidgets
set(wxWidgets_CONFIGURATION mswu)
find_package(wxWidgets COMPONENTS core base REQUIRED)
include(${wxWidgets_USE_FILE})

#   PLY conversion from  https://w3.impa.br/~diego/software/rply/
#   copy convert.c from src/rply-1.1.4/etc to src/rply-1.1.4/
    file(COPY src/rply-1.1.4/etc/convert.c DESTINATION src/rply-1.1.4/)
    file(GLOB_RECURSE src/rply-1.1.4 *.c .h)
    add_executable(ConvertPLYtoBIN src/rply-1.1.4/convert.c src/rply-1.1.4/rply.c )

  find_package(BLAS)

   if ( BLAS_FOUND )

   else()

    set(BLAS_LIBRARIES "/usr/local/lib/libblas.so")
 
   endif()

 find_package(LAPACK)

   if ( LAPACK_FOUND )

   else()

    set(LAPACK_LIBRARIES "/usr/lib/i386-linux-gnu/lapack/liblapack.so.3")
  
   endif()

 find_package(SuperLU)

   if ( SUPERLU_FOUND )

   else()

    set(SUPERLU_LIBRARIES "/usr/local/lib/libsuperlu.so")
  
   endif()

find_package(OpenCL)

#add_executable(myexe ${CMAKE_CURRENT_SOURCE_DIR}/myexe.f90)

# set(CMAKE_BUILD_TYPE RelWithDebInfo) #debugging with optimized code
# set(CMAKE_BUILD_TYPE Release) # no debug or bounds check
set(CMAKE_BUILD_TYPE Debug) #this include debugging with addition of bounds flags as specified below
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${omp}")

#
# Compile.
#

file(GLOB_RECURSE lib_includes  src/lib90 *.fi)
file(GLOB_RECURSE lib_sources90  src/lib90 *.f90 *.for)
file(GLOB_RECURSE mod_sources90  src/modules *.f90)
#file(GLOB_RECURSE handh_sources src/Chapter2 *.f *.f90)
file( GLOB app_sources src/lib90/*.for )
add_library( modules ${mod_sources90})
add_library( splia ${lib_includes} ${lib_sources90})
add_executable(${PROJECT_NAME} src/lib90/janus.f90)
#add_executable(testme1 testme1.f90)

#   need to compile superlu or can add link to /usr/lib/libsuperlu.so.X to /usr/local/lib/libsuperlu.so

if ( OpenCL_FOUND )
 target_link_libraries(${PROJECT_NAME} splia modules ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OpenCL_LIBRARIES} ${SUPERLU_LIBRARIES} )
else()
 target_link_libraries(${PROJECT_NAME} splia modules ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${SUPERLU_LIBRARIES} )
endif()



