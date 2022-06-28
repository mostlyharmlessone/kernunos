cmake_minimum_required(VERSION 3.16)
cmake_policy(VERSION 3.16)

project(Janus)

set(CMAKE_CXX_STANDARD 17) # doesn't apply to win32
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS ON)


enable_language(Fortran)
enable_language(CXX)
enable_language(C)
#enable_language(CUDA)
# /opt/cuda/bin/nvcc is the compiler
enable_language(ASM)

list(PREPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# added much of below from https://github.com/JoeyDeVries/LearnOpenGL/blob/master/CMakeLists.txt

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/modules/")

if(WIN32)
	set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/bin)
endif(WIN32)

link_directories(${CMAKE_SOURCE_DIR}/lib)

# find the required packages
find_package(GLM REQUIRED)
message(STATUS "GLM included at ${GLM_INCLUDE_DIR}")
find_package(GLFW3 REQUIRED)
message(STATUS "Found GLFW3 in ${GLFW3_INCLUDE_DIR}")
find_package(ASSIMP REQUIRED)
message(STATUS "Found ASSIMP in ${ASSIMP_INCLUDE_DIR}")
# find_package(SOIL REQUIRED)
# message(STATUS "Found SOIL in ${SOIL_INCLUDE_DIR}")
 find_package(GLEW REQUIRED)
 message(STATUS "Found GLEW in ${GLEW_INCLUDE_DIR}")
if(APPLE)
  find_package(Freetype REQUIRED)
  message(STATUS "Found Freetype in ${FREETYPE_INCLUDE_DIRS}")
endif(APPLE)

if(WIN32)
  set(LIBS glfw3 opengl32 assimp freetype irrKlang)
  add_definitions(-D_CRT_SECURE_NO_WARNINGS)
elseif(UNIX AND NOT APPLE)
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")
  find_package(OpenGL REQUIRED)
  add_definitions(${OPENGL_DEFINITIONS})
  find_package(X11 REQUIRED)
  # note that the order is important for setting the libs
  # use pkg-config --libs $(pkg-config --print-requires --print-requires-private glfw3) in a terminal to confirm
  set(LIBS ${GLFW3_LIBRARY} X11 Xrandr Xinerama Xi Xxf86vm Xcursor GL dl pthread freetype ${ASSIMP_LIBRARY})
  set (CMAKE_CXX_LINK_EXECUTABLE "${CMAKE_CXX_LINK_EXECUTABLE} -ldl")
elseif(APPLE)
  INCLUDE_DIRECTORIES(/System/Library/Frameworks)
  FIND_LIBRARY(COCOA_LIBRARY Cocoa)
  FIND_LIBRARY(OpenGL_LIBRARY OpenGL)
  FIND_LIBRARY(IOKit_LIBRARY IOKit)
  FIND_LIBRARY(CoreVideo_LIBRARY CoreVideo)
  MARK_AS_ADVANCED(COCOA_LIBRARY OpenGL_LIBRARY)
  SET(APPLE_LIBS ${COCOA_LIBRARY} ${IOKit_LIBRARY} ${OpenGL_LIBRARY} ${CoreVideo_LIBRARY})
  SET(APPLE_LIBS ${APPLE_LIBS} ${GLFW3_LIBRARY} ${ASSIMP_LIBRARY} ${FREETYPE_LIBRARIES})
  set(LIBS ${LIBS} ${APPLE_LIBS})
else()
  set(LIBS )
endif(WIN32)


# set(CHAPTERS
#    1.getting_started
#    2.lighting
# )

# set(1.getting_started
#    1.1.hello_window
#    1.2.hello_window_clear
#    2.1.hello_triangle
# )

configure_file(configuration/root_directory.h configuration/root_directory.h)
include_directories(${CMAKE_BINARY_DIR}/configuration)


# Libigl
 option(LIBIGL_GLFW "Build target igl::glfw" ON)
# option (LIBIGL_WITH_OPENGL "Use OpenGL" ON)
# option (LIBIGL_WITH_OPENGL_GLFW "Use GLFW" ON)
# option (LIBIGL_WITH_OPENGL_GLFW_IMGUI "Use ImGui" ON)
 include(libigl)

# Add your project files
#file(GLOB SRC_FILES_2 src/cxx/view2/*.cpp)
#add_executable(view2 ${SRC_FILES_2})
#target_link_libraries(view2 PUBLIC igl::glfw)

#file(GLOB SRC_FILES_1 src/cxx/view1/*.cpp)
#add_executable(view1 ${SRC_FILES_1})
#target_link_libraries(view1 PUBLIC igl::glfw)

#file(GLOB SRC_FILES src/cxx/view/*.cpp src/cxx/view/*.h)
#add_executable(view ${SRC_FILES})
#target_link_libraries(view PUBLIC igl::glfw)
  

set (CMAKE_Fortran_COMPILER_ID "gfortran")

set_property(GLOBAL PROPERTY GLOBAL_DEPENDS_DEBUG_MODE 1) #second option shows dependencies

# after wiping CMakeFiles folder run the following to reset

set (CMAKE_PREFIX_PATH "/usr/local")

set (FC "gfortran") 
#cmake -DCMAKE_Fortran_COMPILER_ID=gfortran ./
if(CMAKE_Fortran_COMPILER_ID MATCHES "gfortran")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
#     set(dialect "-std=gnu")
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
   if ( OPENCL_FOUND )
   else()
    set(OPENCL_LIBRARIES "/usr/lib/libOpenCL.so")
   endif()
   
   
macro(makeLink src dest target)
  add_custom_command(TARGET ${target} POST_BUILD COMMAND ${CMAKE_COMMAND} -E create_symlink ${src} ${dest}  DEPENDS  ${dest} COMMENT "mklink ${src} -> ${dest}")
endmacro()
   
function(create_project_from_sources chapter demo)
	file(GLOB SOURCE
            "src/${chapter}/${demo}/*.h"
            "src/${chapter}/${demo}/*.cpp"
            "src/${chapter}/${demo}/*.vs"
            "src/${chapter}/${demo}/*.fs"
    )
	if (demo STREQUAL "")
		SET(replaced "")
		string(REPLACE "/" "_" replaced ${chapter})
		set(NAME ${replaced})
	else()
		set(NAME "${chapter}__${demo}")
	endif()
    add_executable(${NAME} ${SOURCE})
    target_link_libraries(${NAME} ${LIBS})
    if(MSVC)
		target_compile_options(${NAME} PRIVATE /std:c++17 /MP)
        target_link_options(${NAME} PUBLIC /ignore:4099)
    endif(MSVC)
    if(WIN32)
        set_target_properties(${NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/${chapter}")
        set_target_properties(${NAME} PROPERTIES VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/${chapter}/Debug")
    elseif(UNIX AND NOT APPLE)
        set_target_properties(${NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/${chapter}")
    elseif(APPLE)
        set_target_properties(${NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/${chapter}")
        set_target_properties(${NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_DEBUG "${CMAKE_SOURCE_DIR}/bin/${chapter}")
        set_target_properties(${NAME} PROPERTIES RUNTIME_OUTPUT_DIRECTORY_RELEASE "${CMAKE_SOURCE_DIR}/bin/${chapter}")
    endif(WIN32)
    # copy shader files to build directory
    file(GLOB SHADERS
             "src/${chapter}/${demo}/*.vs"
             "src/${chapter}/${demo}/*.fs"
    )
	# copy dlls
	file(GLOB DLLS "dlls/*.dll")
    foreach(SHADER ${SHADERS})
        if(WIN32)
            # configure_file(${SHADER} "test")
            add_custom_command(TARGET ${NAME} PRE_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${SHADER} $<TARGET_FILE_DIR:${NAME}>)
			add_custom_command(TARGET ${NAME} PRE_BUILD COMMAND ${CMAKE_COMMAND} -E copy ${DLLS} $<TARGET_FILE_DIR:${NAME}>)
        elseif(UNIX AND NOT APPLE)
            file(COPY ${SHADER} DESTINATION ${CMAKE_SOURCE_DIR}/bin/${chapter})
        elseif(APPLE)
            # create symbolic link for *.vs *.fs *.gs
            get_filename_component(SHADERNAME ${SHADER} NAME)
            makeLink(${SHADER} ${CMAKE_SOURCE_DIR}/bin/${chapter}/${SHADERNAME} ${NAME})
        endif(WIN32)
    endforeach(SHADER)
    # if compiling for visual studio, also use configure file for each project (specifically to set up working directory)
    if(MSVC)
        configure_file(${CMAKE_SOURCE_DIR}/configuration/visualstudio.vcxproj.user.in ${CMAKE_CURRENT_BINARY_DIR}/${NAME}.vcxproj.user @ONLY)
    endif(MSVC)
endfunction()   
   

#viennaCL

# ViennaCL example
  add_executable(viennacl src/cxx/vienna/viennacl-info.cpp)  
  target_link_libraries(viennacl ${OPENCL_LIBRARIES} ) 
  add_executable(sparse src/cxx/vienna/sparse.cpp)  
  target_link_libraries(sparse ${OPENCL_LIBRARIES} )
  add_executable(blasexample src/cxx/vienna/blas3.cpp)  
  target_link_libraries(blasexample ${OPENCL_LIBRARIES} )

# set(CMAKE_BUILD_TYPE RelWithDebInfo) #debugging with optimized code
# set(CMAKE_BUILD_TYPE Release) # no debug or bounds check
set(CMAKE_BUILD_TYPE Debug) #this include debugging with addition of bounds flags as specified below
set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${omp}")

#
# Compile.

#   PLY conversion from  https://w3.impa.br/~diego/software/rply/
#                        https://github.com/diegonehab/rply
#   copy convert.c from rply/etc to rply and both to src/cxx/rply
    file(COPY rply/etc/convert.c DESTINATION src/cxx/rply)
    file(COPY rply/rply.c rply/rply.h rply/rplyfile.h DESTINATION src/cxx/rply)
    file(GLOB_RECURSE src/cxx/rply *.c .h)
    add_executable(ConvertPLYtoBIN src/cxx/rply/convert.c src/cxx/rply/rply.c )

#   C source included in Janus
#   rply conversion 
    file(GLOB c_sources  src/cxx/rply/convertplytobin.c src/cxx/rply/rply.c src/cxx/rply *.h )
#   Fortran Source
    file(GLOB_RECURSE lib_includes  src/lib90 *.fi)
    file(GLOB_RECURSE lib_sources90  src/lib90 *.f90 *.for)
    file(GLOB_RECURSE mod_sources90  src/modules *.f90)
#    file(GLOB_RECURSE handh_sources src/Chapter2 *.f *.f90)
#    file( GLOB app_sources src/lib90/*.for )
    add_library( c_includes ${c_sources})
    add_library( modules ${mod_sources90})
    add_library( splia ${lib_includes} ${lib_sources90})
    add_executable(${PROJECT_NAME} src/lib90/janus.f90 )
#    add_executable(testme1 testme1.f90)

if ( OPENCL_FOUND )
 target_link_libraries(${PROJECT_NAME} splia modules c_includes ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${OPENCL_LIBRARIES} ${SUPERLU_LIBRARIES} )
else()
 target_link_libraries(${PROJECT_NAME} splia modules c_includes ${BLAS_LIBRARIES} ${LAPACK_LIBRARIES} ${SUPERLU_LIBRARIES} )
endif()

## then create a project file per tutorial
# foreach(CHAPTER ${CHAPTERS})
#    foreach(DEMO ${${CHAPTER}})
#		create_project_from_sources(${CHAPTER} ${DEMO})
#    endforeach(DEMO)
# endforeach(CHAPTER)

include_directories(${CMAKE_SOURCE_DIR}/includes)

