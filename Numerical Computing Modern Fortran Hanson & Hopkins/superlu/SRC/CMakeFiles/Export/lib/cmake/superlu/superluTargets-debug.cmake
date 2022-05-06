#----------------------------------------------------------------
# Generated CMake target import file for configuration "DEBUG".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "superlu::superlu" for configuration "DEBUG"
set_property(TARGET superlu::superlu APPEND PROPERTY IMPORTED_CONFIGURATIONS DEBUG)
set_target_properties(superlu::superlu PROPERTIES
  IMPORTED_LOCATION_DEBUG "${_IMPORT_PREFIX}/lib/libsuperlu.so.5.2.1"
  IMPORTED_SONAME_DEBUG "libsuperlu.so.5"
  )

list(APPEND _IMPORT_CHECK_TARGETS superlu::superlu )
list(APPEND _IMPORT_CHECK_FILES_FOR_superlu::superlu "${_IMPORT_PREFIX}/lib/libsuperlu.so.5.2.1" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
