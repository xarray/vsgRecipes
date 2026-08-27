#include "3dgs.h"
#include <vsg/app/Camera.h>
#include <vsg/app/CloseHandler.h>
#include <vsg/app/CommandGraph.h>
#include <vsg/app/RenderGraph.h>
#include <vsg/app/Trackball.h>
#include <vsg/app/View.h>
#include <vsg/app/Viewer.h>
#include <vsg/app/Window.h>
#include <vsg/app/WindowTraits.h>
#include <vsg/nodes/Group.h>
#include <vsg/io/Options.h>
#include <vsg/state/ResourceHints.h>

#include <algorithm>
#include <cctype>
#include <cstdlib>
#include <iostream>

namespace
{
    void printUsage(std::ostream& out)
    {
        out << "vsgload3dgs : view 3D Gaussian Splatting data (.splat / .ply)\n\n";
        out << "Usage:\n";
        out << "    vsgload3dgs [--fov deg] [--near n] [--far f] [file]\n\n";
        out << "Options:\n";
        out << "    --fov deg    vertical field of view in degrees (default 60)\n";
        out << "    --near n     near plane distance (default 0.1)\n";
        out << "    --far f      far plane distance (default 1000.0)\n";
        out << "    -h, --help   print this message\n";
    }
}

int main(int argc, char** argv)
{
    std::string filename;
    double fovY = 60.0, nearPlane = 0.1, farPlane = 1000.0;
    for (int i = 1; i < argc; ++i)
    {
        std::string arg = argv[i];
        if (arg == "-h" || arg == "--help")
        {
            printUsage(std::cout);
            return 0;
        }
        else if (arg == "--fov" && i + 1 < argc) fovY = std::atof(argv[++i]);
        else if (arg == "--near" && i + 1 < argc) nearPlane = std::atof(argv[++i]);
        else if (arg == "--far" && i + 1 < argc) farPlane = std::atof(argv[++i]);
        else if (arg.empty() || arg[0] != '-') filename = arg;
    }

    if (filename.empty())
    {
        std::cout << "No input file specified, trying 'data/3dgs_parrot.splat'\n";
        filename = "data/3dgs_parrot.splat";
    }

    // load data
    vsg::ref_ptr<vsg3dgs::GaussianGeometry> geometry;
    std::string ext = filename.substr(filename.find_last_of('.') + 1);
    std::transform(ext.begin(), ext.end(), ext.begin(), [](unsigned char c)
                   { return static_cast<char>(std::tolower(c)); });

    if (ext == "splat") geometry = vsg3dgs::loadSplatFile(filename);
    else if (ext == "ply") geometry = vsg3dgs::loadPlyFile(filename);
    else
    {
        std::cerr << "Unsupported file extension: '" << ext << "'. Supported: .splat, .ply\n";
        printUsage(std::cerr);
        return 1;
    }

    if (!geometry || !geometry->ready())
    {
        std::cerr << "Failed to load " << filename << std::endl;
        return 1;
    }
    std::cout << "Loaded " << geometry->getNumSplats() << " splats from " << filename << std::endl;
    std::cout << "Bounds: min=(" << geometry->getBounds().min.x << ", " << geometry->getBounds().min.y << ", " << geometry->getBounds().min.z
              << ") max=(" << geometry->getBounds().max.x << ", " << geometry->getBounds().max.y << ", " << geometry->getBounds().max.z << ")\n";
    std::cout << "SH degrees: " << geometry->getShDegrees() << std::endl;

    // create window
    auto windowTraits = vsg::WindowTraits::create();
    windowTraits->width = 1024;
    windowTraits->height = 768;
    windowTraits->windowTitle = "vsg 3DGS: " + filename;
    auto window = vsg::Window::create(windowTraits);
    if (!window)
    {
        std::cerr << "Failed to create window" << std::endl;
        return 1;
    }

    // frame camera on geometry
    vsg::dvec3 center = geometry->getCenter();
    double radius = std::max(geometry->getRadius(), 1.0);
    auto lookAt = vsg::LookAt::create(
        center + vsg::dvec3(0.0, -radius * 2.5, radius * 1.5),
        center, vsg::dvec3(0.0, 0.0, 1.0));

    double aspect = static_cast<double>(window->extent2D().width) / static_cast<double>(std::max(window->extent2D().height, 1u));
    auto perspective = vsg::Perspective::create(fovY, aspect, nearPlane, farPlane);
    auto camera = vsg::Camera::create(perspective, lookAt, vsg::ViewportState::create(window->extent2D()));

    // scene graph
    auto root = vsg::Group::create();
    root->addChild(geometry);

    auto view = vsg::View::create(camera);
    view->addChild(root);

    auto renderGraph = vsg::RenderGraph::create(window, view);
    auto commandGraph = vsg::CommandGraph::create(window, renderGraph);

    auto viewer = vsg::Viewer::create();
    viewer->addWindow(window);
    viewer->assignRecordAndSubmitTaskAndPresentation({commandGraph});
    viewer->compile(vsg::ResourceHints::create());

    auto trackball = vsg::Trackball::create(camera);
    trackball->addWindow(window);
    viewer->addEventHandler(trackball);
    viewer->addEventHandler(vsg::CloseHandler::create(viewer));

    // render loop
    const vsg::dmat4 modelMatrix; // identity
    vsg::ivec2 screenSize(window->extent2D().width, window->extent2D().height);

    while (viewer->advanceToNextFrame())
    {
        viewer->handleEvents();
        viewer->update();

        // update sorting and matrix uniform each frame
        vsg::dmat4 viewMatrix = view->camera->viewMatrix->transform();
        vsg::dmat4 projectionMatrix = view->camera->projectionMatrix->transform();
        geometry->sort(modelMatrix, viewMatrix);
        geometry->update(viewMatrix, projectionMatrix, screenSize,
                         static_cast<float>(nearPlane), static_cast<float>(farPlane), modelMatrix);

        viewer->recordAndSubmit();
        viewer->present();
    }

    return 0;
}