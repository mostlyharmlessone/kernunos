#----------------------------------------------------------------
# Generated CMake target import file for configuration "RELEASE".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "coretran" for configuration "RELEASE"
set_property(TARGET coretran APPEND PROPERTY IMPORTED_CONFIGURATIONS RELEASE)
set_target_properties(coretran PROPERTIES
  IMPORTED_LOCATION_RELEASE "${_IMPORT_PREFIX}/lib/libcoretran.so"
  IMPORTED_SONAME_RELEASE "libcoretran.so"
  )

list(APPEND _cmake_import_check_targets coretran )
list(APPEND _cmake_import_check_files_for_coretran "${_IMPORT_PREFIX}/lib/libcoretran.so" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
