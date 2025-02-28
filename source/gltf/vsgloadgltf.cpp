#include <vsg/all.h>
#include <algorithm>
#include <chrono>
#include <iostream>
#include <thread>
#include "gltf.h"

vsg::ref_ptr<vsg::Node> createTextureQuad(vsg::ref_ptr<vsg::Data> sourceData,
                                          vsg::ref_ptr<vsg::Options> options)
{
    vsg::StateInfo state;
    state.image = sourceData;
    state.lighting = false;

    vsg::GeometryInfo geom;
    geom.dy.set(0.0f, 0.0f, 1.0f);
    geom.dz.set(0.0f, -1.0f, 0.0f);

    vsg::ref_ptr<vsg::Builder> builder = vsg::Builder::create();
    builder->options = options;
    return builder->createQuad(geom, state);
}

int main(int argc, char** argv)
{
    vsg::CommandLine arguments(&argc, argv);

    // set up defaults and read command line arguments to override them
    vsg::ref_ptr<vsg::Options> options = vsg::Options::create();
    options->readerWriters.push_back(vsg::ref_ptr<vsg::ReaderWriter>(new vsgRecipes::gltf()));
    options->sharedObjects = vsg::SharedObjects::create();
    options->fileCache = vsg::getEnv("VSG_FILE_CACHE");
    options->paths = vsg::getEnvPaths("VSG_FILE_PATH");
    arguments.read(options);

    vsg::ref_ptr<vsg::WindowTraits> windowTraits = vsg::WindowTraits::create();
    windowTraits->windowTitle = "vsgRecipes: load gltf";
    windowTraits->debugLayer = arguments.read({"--debug", "-d"});
    windowTraits->apiDumpLayer = arguments.read({"--api", "-a"});
    arguments.read("--screen", windowTraits->screenNum);
    arguments.read("--display", windowTraits->display);
    if (arguments.read({"--fullscreen", "--fs"})) windowTraits->fullscreen = true;
    if (arguments.read({"--window", "-w"}, windowTraits->width, windowTraits->height))
        windowTraits->fullscreen = false;
    if (arguments.read("--IMMEDIATE"))
        windowTraits->swapchainPreferences.presentMode = VK_PRESENT_MODE_IMMEDIATE_KHR;
    
    if (arguments.errors()) return arguments.writeErrorMessages(std::cerr);
    if (argc <= 1)
    {
        std::cout << "Please specify a 3d model or image file on the command line." << std::endl;
        return 1;
    }

    // read any vsg files
    vsg::ref_ptr<vsg::Group> group = vsg::Group::create();
    for (int i = 1; i < argc; ++i)
    {
        vsg::ref_ptr<vsg::Object> object = vsg::read(arguments[i], options);
        if (vsg::ref_ptr<vsg::Node> node = object.cast<vsg::Node>())
            group->addChild(node);
        else if (vsg::ref_ptr<vsg::Data> data = object.cast<vsg::Data>())
        {
            if (vsg::ref_ptr<vsg::Node> textureQuad = createTextureQuad(data, options))
                group->addChild(textureQuad);
        }
        else
            std::cout << "Unable to load file " << arguments[i] << std::endl;
    }

    if (group->children.empty())
    {
        std::cout << "Please specify a 3d model or image file on the command line." << std::endl;
        return 1;
    }

    vsg::ref_ptr<vsg::Node> scene;
    if (group->children.size() == 1)
        scene = group->children[0];
    else
        scene = group;

    // create the viewer and assign window(s) to it
    vsg::ref_ptr<vsg::Window> window = vsg::Window::create(windowTraits);
    if (!window)
    {
        std::cout << "Could not create window." << std::endl;
        return 1;
    }

    vsg::ref_ptr<vsg::Viewer> viewer = vsg::Viewer::create();
    viewer->addWindow(window);

    // compute the bounds of the scene graph to help position camera
    vsg::ComputeBounds computeBounds;
    scene->accept(computeBounds);

    vsg::dvec3 centre = (computeBounds.bounds.min + computeBounds.bounds.max) * 0.5;
    double radius = vsg::length(computeBounds.bounds.max - computeBounds.bounds.min) * 0.6;
    double nearFarRatio = arguments.value<double>(0.001, "--nfr");

    // set up the camera
    vsg::ref_ptr<vsg::LookAt> lookAt = vsg::LookAt::create(
        centre + vsg::dvec3(0.0, -radius * 3.5, 0.0), centre, vsg::dvec3(0.0, 0.0, 1.0));
    vsg::ref_ptr<vsg::ProjectionMatrix> perspective = vsg::Perspective::create(
        30.0, static_cast<double>(window->extent2D().width) / static_cast<double>(window->extent2D().height),
        nearFarRatio * radius, radius * 4.5);
    vsg::ref_ptr<vsg::Camera> camera = vsg::Camera::create(
        perspective, lookAt, vsg::ViewportState::create(window->extent2D()));

    // add viewer handlers and create command graph for scene
    viewer->addEventHandler(vsg::Trackball::create(camera, nullptr));
    viewer->addEventHandler(vsg::CloseHandler::create(viewer));

    vsg::ref_ptr<vsg::CommandGraph> commandGraph = vsg::createCommandGraphForView(window, camera, scene);
    viewer->assignRecordAndSubmitTaskAndPresentation({commandGraph});
    viewer->compile();

    // rendering main loop
    viewer->start_point() = vsg::clock::now();
    while (viewer->advanceToNextFrame())
    {
        viewer->handleEvents();
        viewer->update();
        viewer->recordAndSubmit();
        viewer->present();
    }
    return 0;
}
