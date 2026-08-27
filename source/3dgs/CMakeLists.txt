set(SOURCES
    vsgload3dgs.cpp
    3dgs.h 3dgs.cpp
    3dgs_shaders.h
)

add_executable(vsgload3dgs ${SOURCES})
target_link_libraries(vsgload3dgs vsg::vsg)
install(TARGETS vsgload3dgs RUNTIME DESTINATION bin)
