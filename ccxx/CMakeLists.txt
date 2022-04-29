cmake_minimum_required(VERSION 3.17)

if( CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR )
        message( FATAL "@todo: DealWithMe")
        set(CMAKE_TOOLCHAIN_FILE ${CMAKE_CURRENT_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake CACHE STRING "Vcpkg toolchain file")
        project(ccxx)

        set(CMAKE_CXX_STANDARD 20 CACHE STRING "C++ standard to conform to")
        set(CMAKE_CXX_STANDARD_REQUIRED YES)
        set(CMAKE_CXX_EXTENSIONS NO)
      
        cmake_policy(SET CMP0098 NEW)
              
        find_package(LLVM CONFIG REQUIRED)
        find_package(Clang CONFIG REQUIRED)
        list(APPEND CMAKE_MODULE_PATH ${LLVM_DIR})

        include(AddLLVM)
        include(TableGen)
        include(HandleLLVMOptions)
        include(VersionFromVCS)
        include(LLVMDistributionSupport)
        
        include_directories("${LLVM_BINARY_DIR}/include" "${LLVM_MAIN_INCLUDE_DIR}")
        link_directories("${LLVM_LIBRARY_DIR}")
      
        set( CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
        set( CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )
        set( CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib${LLVM_LIBDIR_SUFFIX} )

        add_definitions(${LLVM_DEFINITIONS})

else()
        include_directories( ${CLANG_SOURCE_DIR}/include )
endif()

set (CMAKE_INCLUDE_CURRENT_DIR ON)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/include)

add_subdirectory(tools/TableGen)
add_subdirectory(include/ast)

get_property(CCXX_TABLEGEN_TARGETS GLOBAL PROPERTY CCXX_TABLEGEN_TARGETS)
add_custom_target(ccxx-tablegen-targets
        DEPENDS
        ${CCXX_TABLEGEN_TARGETS})

set_target_properties(ccxx-tablegen-targets PROPERTIES FOLDER "Misc")
list(APPEND LLVM_COMMON_DEPENDS ccxx-tablegen-targets)

add_llvm_executable (ccxx
        ccxx.cc
        )

llvm_map_components_to_libnames(llvm_libs support core X86)
target_link_libraries(ccxx PRIVATE ${llvm_libs} ccxx_lex clangBasic)

if (MSVC)
    target_compile_options(ccxx PRIVATE /wd4624 /wd4996 /wd4141)
endif()
target_compile_features(ccxx PRIVATE cxx_std_20)


add_subdirectory(src)

# add_subdirectory(test)
add_subdirectory(stdlib)

# Assign files to source groups for visual studio, which would otherwise lump  all source files together
if (MSVC)
        get_target_property(CCXX_SOURCES ccxx SOURCES)
        foreach(_source IN ITEMS ${CCXX_SOURCES})
                if (IS_ABSOLUTE "${_source}")
                        file(RELATIVE_PATH _source_rel "${CMAKE_CURRENT_SOURCE_DIR}" "${_source}")
                else()
                        set(_source_rel "${_source}")
                endif()
                get_filename_component(_source_path "${_source_rel}" PATH)
                string(REPLACE "/" "\\" _source_path_msvc "${_source_path}")
                source_group("${_source_path_msvc}" FILES "${_source}")
        endforeach()
endif()