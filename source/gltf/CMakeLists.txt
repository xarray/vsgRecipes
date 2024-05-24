set(SOURCES
    vsgloadgltf.cpp
    gltf.h gltf.cpp
)

add_executable(vsgloadgltf ${SOURCES})
target_link_libraries(vsgloadgltf vsg::vsg)
install(TARGETS vsgloadgltf RUNTIME DESTINATION bin)
