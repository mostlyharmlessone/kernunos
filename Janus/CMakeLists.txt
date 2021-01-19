
cmake_minimum_required(VERSION 3.1...3.15)
project(Janus)
enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
enable_language(ASM)

set (CMAKE_Fortran_COMPILER_ID "gfortran")

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 1) #second option shows dependencies

# after wiping CMakeFiles folder run the following to reset

set (FC "gfortran") 
#cmake -DCMAKE_Fortran_COMPILER_ID=gfortran ./
if(CMAKE_Fortran_COMPILER_ID MATCHES "gfortran")
#    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
     set(dialect "-std=gnu")
     set(bounds "-fbounds-check")
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

Set (CGAL_DO_NOT_WARN_ABOUT_CMAKE_BUILD_TYPE "TRUE")

# libigl
 option (LIBIGL_WITH_OPENGL "Use OpenGL" ON)
 option (LIBIGL_WITH_OPENGL_GLFW "Use GLFW" ON)
 option (LIBIGL_WITH_OPENGL_GLFW_IMGUI "Use ImGui" ON)
# option (LIBIGL_WITH_CGAL "use with CGAL" ON)

find_package(CGAL QUIET)

if ( CGAL_FOUND )

  # use Eigen
  find_package(Eigen3 3.1.0) #(requires 3.1.0 or greater)
  if (EIGEN3_FOUND)
    include( ${EIGEN3_USE_FILE} )
    # Link with Boost.ProgramOptions (optional)
    find_package(Boost QUIET COMPONENTS program_options)
    if(Boost_PROGRAM_OPTIONS_FOUND)
      if(TARGET Boost::program_options)
        set(Boost_PROGRAM_OPTIONS_LIBRARY Boost::program_options)
      endif()
      if( CGAL_AUTO_LINK_ENABLED )
        message( STATUS "Boost.ProgramOptions library: found" )
      else()
        message( STATUS "Boost.ProgramOptions library: ${Boost_PROGRAM_OPTIONS_LIBRARY}" )
      endif()
      add_definitions( "-DCGAL_USE_BOOST_PROGRAM_OPTIONS" )
      list(APPEND CGAL_3RD_PARTY_LIBRARIES ${Boost_PROGRAM_OPTIONS_LIBRARY})
   endif()

    create_single_source_cgal_program( "Mesh_estimation.cpp" )
    create_single_source_cgal_program( "Single_estimation.cpp" )
#   use ./Mesh_estimation -f elevation.off -d2 -m2 -a2 -vtrue     
#       ./Compute_Ridges_Umbilics -f elevation.off -d4 -m4 -a4 -t3 -v0     
    find_package(LIBIGL)

   if ( LIBIGL_FOUND )
    file(GLOB_RECURSE src/cxx *.cpp)
    add_executable(view2 src/cxx/main.cpp)
    target_include_directories(view2 PRIVATE "${GLAD_DIR}/include")    
    target_link_libraries(view2 igl::core igl::opengl_glfw "glad")
    add_executable(view src/cxx/view.cpp)
    target_include_directories(view PRIVATE "${GLAD_DIR}/include")    
    target_link_libraries(view igl::core igl::opengl_glfw "glad")   
   endif()     
    
  else()
    message(STATUS "NOTICE: This program requires Eigen 3.1 (or greater) and will not be compiled.")
  endif()

else()

  message(STATUS "NOTICE: This program requires the CGAL library, and will not be compiled.")

endif()

find_package(BLAS)
find_package(LAPACK)
find_package(OpenCL)

# to use LAPACK95 from GIT
#include(FetchContent)
#FetchContent_Declare(
#    lapack95
#    GIT_REPOSITORY https://github.com/scivision/LAPACK95.git
#)

#FetchContent_GetProperties(lapack95)
#if(NOT lapack95_POPULATED)
#    FetchContent_Populate(lapack95)
#    add_subdirectory(${lapack95_SOURCE_DIR})
#endif()

#add_executable(myexe ${CMAKE_CURRENT_SOURCE_DIR}/myexe.f90)
#target_link_libraries(myexe ${LAPACK_LIBRARIES} lapack95)



# set(CMAKE_BUILD_TYPE RelWithDebInfo) #debugging with optimized code
# set(CMAKE_BUILD_TYPE Release) # no debug or bounds check
set(CMAKE_BUILD_TYPE Debug) #this include debugging with addition of bounds flags as specified below
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

#
# Compile.
#

file(GLOB_RECURSE lib_includes  src/lib90 *.fi)
file(GLOB_RECURSE lib_sources90  src/lib90 *.f90 *.for)
file(GLOB_RECURSE mod_sources90  src/modules *.f90)
file( GLOB app_sources src/lib90/*.for )
add_library( modules ${mod_sources90})
add_library( splia ${lib_includes} ${lib_sources90})
add_executable(${PROJECT_NAME} src/lib90/janus.f90)

if ( OpenCL_FOUND )
 target_link_libraries(${PROJECT_NAME} splia modules ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OpenCL_LIBRARIES})
else()
# target_link_libraries(${PROJECT_NAME} splia modules ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} lapack95)
 target_link_libraries(${PROJECT_NAME} splia modules ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES})
endif()



