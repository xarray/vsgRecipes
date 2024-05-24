cmake_minimum_required(VERSION 3.7)
project(vsgRecipes
        VERSION 0.0.1
        DESCRIPTION "Ideas and extensions to make better use of VulkanSceneGraph"
        LANGUAGES CXX C
)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin)
# Change the default build type to Release
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Options are: None Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif(NOT CMAKE_BUILD_TYPE)

# find the VSG
if (VULKAN_SDK)
    set(ENV{VULKAN_SDK} ${VULKAN_SDK})
endif()

find_package(vsg 1.1.4)
vsg_setup_dir_vars()
vsg_setup_build_vars()

# set the use of C++17 globally as all examples require it
set(CMAKE_CXX_STANDARD 17)

# set VSG targets
vsg_add_target_clang_format(
    FILES
        ${CMAKE_SOURCE_DIR}/*/*/*.h
        ${CMAKE_SOURCE_DIR}/*/*/*.cpp
        ${CMAKE_SOURCE_DIR}/*/*/*/*.h
        ${CMAKE_SOURCE_DIR}/*/*/*/*.cpp
)
vsg_add_target_clobber()
vsg_add_target_cppcheck(FILES source/)
vsg_add_target_docs(FILES source/)
vsg_add_target_uninstall()

# install data
install(DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/data/ DESTINATION share/vsgRecipes)

# VSG recipes
add_subdirectory(source/gltf)
vsg_add_feature_summary()
