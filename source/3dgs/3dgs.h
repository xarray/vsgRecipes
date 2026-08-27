#pragma once

#include <vsg/core/Array.h>
#include <vsg/maths/box.h>
#include <vsg/nodes/StateGroup.h>
#include <vsg/nodes/VertexIndexDraw.h>
#include <vsg/state/BindDescriptorSet.h>
#include <vsg/state/ColorBlendState.h>
#include <vsg/state/DepthStencilState.h>
#include <vsg/state/DescriptorBuffer.h>
#include <vsg/state/DescriptorSet.h>
#include <vsg/state/DescriptorSetLayout.h>
#include <vsg/state/GraphicsPipeline.h>
#include <vsg/state/InputAssemblyState.h>
#include <vsg/state/MultisampleState.h>
#include <vsg/state/PipelineLayout.h>
#include <vsg/state/RasterizationState.h>
#include <vsg/state/ShaderModule.h>
#include <vsg/state/ShaderStage.h>
#include <vsg/state/VertexInputState.h>

#include <map>

// Matrix uniform buffer data (std140 layout, matching MatrixUBO in 3dgs_shaders.h)
namespace vsg
{
    struct vsg3dgsMatrixUBO
    {
        mat4 modelMatrix;
        mat4 viewMatrix;
        mat4 projectionMatrix;
        vec2 nearFarPlanes;
        vec2 invScreenResolution;
        float gaussianRenderingMode;
        float _pad[3];
    };
    VSG_array(vsg3dgsMatrixUBOArray, vsg3dgsMatrixUBO);
}

namespace vsg3dgs
{
    // 3D Gaussian Splatting geometry node.
    // Data layout follows the osgVerse reference implementation (SSBO + instanced draw):
    //   binding 0 (storage buffer) : vec4[]  (pos.xyz, alpha)
    //   binding 1 (storage buffer) : uvec2[] (scale.xyz half, SH0.r half)
    //   binding 2 (storage buffer) : uvec2[] (quat.xyz half, SH0.g half)
    //   binding 3 (storage buffer) : uvec2[] (quat.w half, 1, 1, SH0.b half)
    //   binding 4 (storage buffer) : uvec2[] (SH coefficients 1..15, optional)
    //   binding 5 (uniform buffer) : matrices and other rendering parameters
    // The instance index array is passed as a vertex attribute (location 1, instance rate)
    // indexing into the SSBOs above.
    class GaussianGeometry : public vsg::Inherit<vsg::StateGroup, GaussianGeometry>
    {
    public:
        GaussianGeometry();

        // ---- data setting interface (mirrors the OSG reference implementation) ----
        void setShDegrees(int d) { _shDegrees = d; }
        int getShDegrees() const { return _shDegrees; }

        void setPosition(vsg::ref_ptr<vsg::vec3Array> positions);
        void setScaleAndRotation(vsg::ref_ptr<vsg::vec3Array> scales, vsg::ref_ptr<vsg::vec4Array> rotations);
        void setAlpha(vsg::ref_ptr<vsg::floatArray> alphas);
        void setShRed(int i, vsg::ref_ptr<vsg::vec4Array> v);
        void setShGreen(int i, vsg::ref_ptr<vsg::vec4Array> v);
        void setShBlue(int i, vsg::ref_ptr<vsg::vec4Array> v);

        // Pack source data into SSBOs and build the rendering pipeline
        bool finalize();

        size_t getNumSplats() const { return _numSplats; }
        bool ready() const { return _ready; }

        // Bounding box, valid after finalize()
        const vsg::dbox& getBounds() const { return _bounds; }
        vsg::dvec3 getCenter() const { return _bounds.valid() ? (_bounds.min + (_bounds.max - _bounds.min) * 0.5) : vsg::dvec3(); }
        double getRadius() const { return _bounds.valid() ? vsg::length(_bounds.max - _bounds.min) * 0.5 : 1.0; }

        // Update the matrix uniform each frame (view/projection/model, screen size, near/far)
        void update(const vsg::dmat4& viewMatrix, const vsg::dmat4& projectionMatrix,
                    const vsg::ivec2& screenSize, float nearPlane, float farPlane,
                    const vsg::dmat4& modelMatrix = {});
        // Sort splats back-to-front each frame for correct alpha blending
        void sort(const vsg::dmat4& modelMatrix, const vsg::dmat4& viewMatrix);

    protected:
        ~GaussianGeometry() = default;

        void buildPipeline();

        int _shDegrees = 0;
        size_t _numSplats = 0;
        bool _ready = false;
        vsg::dbox _bounds;

        // source data
        vsg::ref_ptr<vsg::vec3Array> _positions;
        vsg::ref_ptr<vsg::vec3Array> _scales;
        vsg::ref_ptr<vsg::vec4Array> _rotations; // xyzw quaternion
        vsg::ref_ptr<vsg::floatArray> _alphas;
        std::map<int, vsg::ref_ptr<vsg::vec4Array>> _shRed, _shGreen, _shBlue;

        // SSBO data (generated in finalize())
        vsg::ref_ptr<vsg::vec4Array> _coreBuffer; // binding 0
        vsg::ref_ptr<vsg::ushortArray> _coreCov0; // binding 1
        vsg::ref_ptr<vsg::ushortArray> _coreCov1; // binding 2
        vsg::ref_ptr<vsg::ushortArray> _coreCov2; // binding 3
        vsg::ref_ptr<vsg::ushortArray> _shcoef;   // binding 4
        vsg::ref_ptr<vsg::vsg3dgsMatrixUBOArray> _matrices; // binding 5

        // instance indices (sorted splat order, instance rate vertex attribute)
        vsg::ref_ptr<vsg::uintArray> _indices;

        // rendering objects
        vsg::ref_ptr<vsg::VertexIndexDraw> _draw;
        vsg::ref_ptr<vsg::DescriptorSet> _descriptorSet;
        vsg::ref_ptr<vsg::PipelineLayout> _pipelineLayout;
        vsg::ref_ptr<vsg::GraphicsPipeline> _pipeline;
    };

    // ---- convenience loaders ----
    // Read a .splat file (32 bytes per splat: pos3f + scale3f + rgba4 + rot4)
    vsg::ref_ptr<GaussianGeometry> loadSplatFile(const std::string& filename);

    // Read a standard 3DGS .ply file (binary_little_endian; FULL_SH enabled when SH coeffs are present)
    vsg::ref_ptr<GaussianGeometry> loadPlyFile(const std::string& filename);
}
