#include "3dgs.h"
#include "3dgs_shaders.h"
#include <vsg/io/Options.h>
#include <vsg/utils/ShaderCompiler.h>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <fstream>
#include <limits>

namespace
{
    // float -> half conversion (compact implementation based on Eigen/HalfFloat)
    union FP32 { std::uint32_t u; float f; };

    std::uint16_t floatToHalf(float ff)
    {
        const FP32 f32infty = {255 << 23};
        const FP32 f16max = {(127 + 16) << 23};
        const FP32 denorm_magic = {((127 - 15) + (23 - 10) + 1) << 23};
        const std::uint32_t sign_mask = 0x80000000u;

        FP32 f; f.f = ff;
        std::uint32_t sign = f.u & sign_mask;
        f.u ^= sign;

        std::uint16_t x = 0;
        if (f.u >= f16max.u)
            x = (f.u > f32infty.u) ? 0x7e00u : 0x7c00u;
        else
        {
            if (f.u < (113 << 23))
            {
                f.f += denorm_magic.f;
                x = static_cast<std::uint16_t>(f.u - denorm_magic.u);
            }
            else
            {
                std::uint32_t mant_odd = (f.u >> 13) & 1;
                f.u += static_cast<std::uint32_t>((15 - 127) << 23) + 0xfffu;
                f.u += mant_odd;
                x = static_cast<std::uint16_t>(f.u >> 13);
            }
        }
        x |= static_cast<std::uint16_t>(sign >> 16);
        return x;
    }

    bool readFileBinary(const std::string& filename, std::vector<char>& data)
    {
        std::ifstream fin(filename, std::ios::binary);
        if (!fin) return false;
        fin.seekg(0, std::ios::end);
        std::streamsize size = fin.tellg();
        fin.seekg(0, std::ios::beg);
        if (size <= 0) return false;
        data.resize(static_cast<size_t>(size));
        fin.read(data.data(), size);
        return fin.good() || fin.eof();
    }
}

namespace vsg3dgs
{
    GaussianGeometry::GaussianGeometry() :
        _draw(vsg::VertexIndexDraw::create())
    {
        children.push_back(_draw);
    }

    void GaussianGeometry::setPosition(vsg::ref_ptr<vsg::vec3Array> positions)
    {
        _positions = positions;
    }

    void GaussianGeometry::setScaleAndRotation(vsg::ref_ptr<vsg::vec3Array> scales, vsg::ref_ptr<vsg::vec4Array> rotations)
    {
        _scales = scales;
        _rotations = rotations;
    }

    void GaussianGeometry::setAlpha(vsg::ref_ptr<vsg::floatArray> alphas)
    {
        _alphas = alphas;
    }

    void GaussianGeometry::setShRed(int i, vsg::ref_ptr<vsg::vec4Array> v) { _shRed[i] = v; }
    void GaussianGeometry::setShGreen(int i, vsg::ref_ptr<vsg::vec4Array> v) { _shGreen[i] = v; }
    void GaussianGeometry::setShBlue(int i, vsg::ref_ptr<vsg::vec4Array> v) { _shBlue[i] = v; }

    bool GaussianGeometry::finalize()
    {
        if (!_positions || _positions->empty()) return false;
        _numSplats = _positions->size();

        // compute bounding box
        _bounds = vsg::dbox();
        for (const auto& p : *_positions) _bounds.add(vsg::dvec3(p.x, p.y, p.z));

        // intermediate per-splat layer data matching the OSG reference implementation
        std::vector<vsg::vec4> layer0(_numSplats), layer1(_numSplats), layer2(_numSplats), layer3(_numSplats);
        for (size_t i = 0; i < _numSplats; ++i)
        {
            const auto& p = (*_positions)[i];
            float a = _alphas ? (*_alphas)[i] : 1.0f;
            layer0[i] = vsg::vec4(p.x, p.y, p.z, a);

            if (_scales && _rotations && i < _scales->size() && i < _rotations->size())
            {
                const auto& s = (*_scales)[i];
                const auto& q = (*_rotations)[i];
                layer1[i] = vsg::vec4(s.x, s.y, s.z, 0.0f);
                layer2[i] = vsg::vec4(q.x, q.y, q.z, 0.0f);
                layer3[i] = vsg::vec4(q.w, 1.0f, 1.0f, 0.0f);
            }
            else
            {
                layer1[i] = vsg::vec4(1.0f, 1.0f, 1.0f, 0.0f);
                layer2[i] = vsg::vec4(0.0f, 0.0f, 0.0f, 0.0f);
                layer3[i] = vsg::vec4(1.0f, 1.0f, 1.0f, 0.0f);
            }
        }

        // SH term 0 (baseColor) goes into the w component of layer1/2/3
        auto setSH0 = [&](const std::map<int, vsg::ref_ptr<vsg::vec4Array>>& sh, std::vector<vsg::vec4>& layer) {
            auto itr = sh.find(0);
            if (itr != sh.end() && itr->second)
            {
                size_t n = std::min(_numSplats, static_cast<size_t>(itr->second->size()));
                for (size_t i = 0; i < n; ++i) layer[i].w = (*itr->second)[i].x;
            }
        };
        setSH0(_shRed, layer1);
        setSH0(_shGreen, layer2);
        setSH0(_shBlue, layer3);

        // pack binding 0: pos.xyz + alpha
        _coreBuffer = vsg::vec4Array::create(static_cast<uint32_t>(_numSplats));
        std::memcpy(_coreBuffer->data(), layer0.data(), _numSplats * sizeof(vsg::vec4));

        // pack binding 1/2/3: half4
        _coreCov0 = vsg::ushortArray::create(static_cast<uint32_t>(_numSplats * 4));
        _coreCov1 = vsg::ushortArray::create(static_cast<uint32_t>(_numSplats * 4));
        _coreCov2 = vsg::ushortArray::create(static_cast<uint32_t>(_numSplats * 4));
        for (size_t i = 0; i < _numSplats; ++i)
        {
            for (int k = 0; k < 4; ++k)
            {
                (*_coreCov0)[i * 4 + k] = floatToHalf(layer1[i][k]);
                (*_coreCov1)[i * 4 + k] = floatToHalf(layer2[i][k]);
                (*_coreCov2)[i * 4 + k] = floatToHalf(layer3[i][k]);
            }
        }

        // pack binding 4: SH coefficients 1..15 (15 uvec2 per splat)
        _shcoef = vsg::ushortArray::create(static_cast<uint32_t>(_numSplats * 15 * 4));
        if (_shDegrees > 0)
        {
            std::vector<std::vector<vsg::vec3>> shData(15, std::vector<vsg::vec3>(_numSplats, vsg::vec3()));
            auto setSH = [&](const std::map<int, vsg::ref_ptr<vsg::vec4Array>>& sh, int comp) {
                for (const auto& [gi, arr] : sh)
                {
                    if (!arr) continue;
                    size_t n = std::min(_numSplats, static_cast<size_t>(arr->size()));
                    for (size_t j = 0; j < n; ++j)
                    {
                        for (int k = 0; k < 4; ++k)
                        {
                            int shIndex = gi * 4 + k; // SH term index (0 already used by baseColor)
                            if (shIndex <= 0 || shIndex > 15) continue;
                            shData[shIndex - 1][j][comp] = (*arr)[j][k];
                        }
                    }
                }
            };
            setSH(_shRed, 0);
            setSH(_shGreen, 1);
            setSH(_shBlue, 2);

            for (size_t i = 0; i < 15; ++i)
            {
                for (size_t j = 0; j < _numSplats; ++j)
                {
                    size_t idx = (j * 15 + i) * 4;
                    (*_shcoef)[idx + 0] = floatToHalf(shData[i][j].x);
                    (*_shcoef)[idx + 1] = floatToHalf(shData[i][j].y);
                    (*_shcoef)[idx + 2] = floatToHalf(shData[i][j].z);
                    (*_shcoef)[idx + 3] = 0;
                }
            }
        }

        // instance indices (initial order, used for sorting)
        _indices = vsg::uintArray::create(static_cast<uint32_t>(_numSplats));
        for (uint32_t i = 0; i < _numSplats; ++i) (*_indices)[i] = i;
        _indices->properties.dataVariance = vsg::DYNAMIC_DATA;

        // matrix uniform buffer
        _matrices = vsg::vsg3dgsMatrixUBOArray::create(1);
        _matrices->properties.dataVariance = vsg::DYNAMIC_DATA;

        // quad vertices + indices
        auto quadVertices = vsg::vec3Array::create(4);
        (*quadVertices)[0] = vsg::vec3(1.0f, 1.0f, 0.0f);
        (*quadVertices)[1] = vsg::vec3(-1.0f, 1.0f, 0.0f);
        (*quadVertices)[2] = vsg::vec3(1.0f, -1.0f, 0.0f);
        (*quadVertices)[3] = vsg::vec3(-1.0f, -1.0f, 0.0f);
        
        auto quadIndices = vsg::ushortArray::create(6);
        (*quadIndices)[0] = 0; (*quadIndices)[1] = 1; (*quadIndices)[2] = 2;
        (*quadIndices)[3] = 1; (*quadIndices)[4] = 3; (*quadIndices)[5] = 2;

        _draw->assignArrays(vsg::DataList{quadVertices, _indices});
        _draw->assignIndices(quadIndices);
        _draw->indexCount = 6;
        _draw->instanceCount = static_cast<uint32_t>(_numSplats);

        buildPipeline();
        _ready = true;
        return true;
    }

    void GaussianGeometry::buildPipeline()
    {
        // shader stages
        auto compileSettings = vsg::ShaderCompileSettings::create();
        compileSettings->defines.insert("USE_INSTANCING");
        if (_shDegrees > 0) compileSettings->defines.insert("FULL_SH");

        auto vertexStage = vsg::ShaderStage::create(VK_SHADER_STAGE_VERTEX_BIT, "main", getGaussianSplattingVertexShader(), compileSettings);
        auto fragmentStage = vsg::ShaderStage::create(VK_SHADER_STAGE_FRAGMENT_BIT, "main", getGaussianSplattingFragmentShader(), compileSettings);

        // descriptor set layout
        auto descriptorSetLayout = vsg::DescriptorSetLayout::create(vsg::DescriptorSetLayoutBindings{
            {0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}, // corePos
            {1, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}, // coreCov0
            {2, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}, // coreCov1
            {3, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}, // coreCov2
            {4, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}, // shcoef
            {5, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}  // matrices
        });

        // descriptor set
        vsg::Descriptors descriptors;
        descriptors.push_back(vsg::DescriptorBuffer::create(_coreBuffer, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER));
        descriptors.push_back(vsg::DescriptorBuffer::create(_coreCov0, 1, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER));
        descriptors.push_back(vsg::DescriptorBuffer::create(_coreCov1, 2, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER));
        descriptors.push_back(vsg::DescriptorBuffer::create(_coreCov2, 3, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER));
        descriptors.push_back(vsg::DescriptorBuffer::create(_shcoef, 4, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER));
        descriptors.push_back(vsg::DescriptorBuffer::create(_matrices, 5, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER));
        _descriptorSet = vsg::DescriptorSet::create(descriptorSetLayout, descriptors);

        // pipeline layout
        _pipelineLayout = vsg::PipelineLayout::create(vsg::DescriptorSetLayouts{descriptorSetLayout}, vsg::PushConstantRanges{});

        // vertex input: binding 0 = quad vertices (vertex rate), binding 1 = instance index (instance rate)
        auto vertexInputState = vsg::VertexInputState::create(
            vsg::VertexInputState::Bindings{
                {0, sizeof(vsg::vec3), VK_VERTEX_INPUT_RATE_VERTEX},
                {1, sizeof(std::uint32_t), VK_VERTEX_INPUT_RATE_INSTANCE}},
            vsg::VertexInputState::Attributes{
                {0, 0, VK_FORMAT_R32G32B32_SFLOAT, 0},
                {1, 1, VK_FORMAT_R32_UINT, 0}});

        auto inputAssemblyState = vsg::InputAssemblyState::create();
        auto multisampleState = vsg::MultisampleState::create();
        
        auto rasterizationState = vsg::RasterizationState::create();
        rasterizationState->cullMode = VK_CULL_MODE_NONE;

        auto depthStencilState = vsg::DepthStencilState::create();
        depthStencilState->depthTestEnable = VK_TRUE;
        depthStencilState->depthWriteEnable = VK_FALSE;
        depthStencilState->depthCompareOp = VK_COMPARE_OP_GREATER; // VSG reverse-z default

        // premultiplied alpha blending, matching glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA) in the reference
        VkPipelineColorBlendAttachmentState colorBlendAttachment = {};
        colorBlendAttachment.blendEnable = VK_TRUE;
        colorBlendAttachment.colorWriteMask = VK_COLOR_COMPONENT_R_BIT |
                                            VK_COLOR_COMPONENT_G_BIT |
                                            VK_COLOR_COMPONENT_B_BIT |
                                            VK_COLOR_COMPONENT_A_BIT;
        colorBlendAttachment.srcColorBlendFactor = VK_BLEND_FACTOR_ONE;
        colorBlendAttachment.dstColorBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
        colorBlendAttachment.colorBlendOp = VK_BLEND_OP_ADD;
        colorBlendAttachment.srcAlphaBlendFactor = VK_BLEND_FACTOR_ONE;
        colorBlendAttachment.dstAlphaBlendFactor = VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA;
        colorBlendAttachment.alphaBlendOp = VK_BLEND_OP_ADD;
        auto colorBlendState = vsg::ColorBlendState::create(
            vsg::ColorBlendState::ColorBlendAttachments{colorBlendAttachment});

        // create pipeline
        vsg::GraphicsPipelineStates pipelineStates;
        pipelineStates.push_back(vertexInputState);
        pipelineStates.push_back(inputAssemblyState);
        pipelineStates.push_back(rasterizationState);
        pipelineStates.push_back(multisampleState);
        pipelineStates.push_back(depthStencilState);
        pipelineStates.push_back(colorBlendState);

        auto shaderStages = vsg::ShaderStages{vertexStage, fragmentStage};
        _pipeline = vsg::GraphicsPipeline::create(_pipelineLayout, shaderStages, pipelineStates);
        stateCommands.push_back(vsg::BindGraphicsPipeline::create(_pipeline));
        stateCommands.push_back(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, _pipelineLayout, 0, _descriptorSet));
    }

    void GaussianGeometry::update(const vsg::dmat4& viewMatrix, const vsg::dmat4& projectionMatrix,
                                  const vsg::ivec2& screenSize, float nearPlane, float farPlane,
                                  const vsg::dmat4& modelMatrix)
    {
        if (!_ready) return;
        auto& m = (*_matrices)[0];
        m.modelMatrix = vsg::mat4(modelMatrix);
        m.viewMatrix = vsg::mat4(viewMatrix);
        m.projectionMatrix = vsg::mat4(projectionMatrix);
        m.nearFarPlanes = vsg::vec2(nearPlane, farPlane);
        m.invScreenResolution = vsg::vec2(screenSize.x > 0 ? 1.0f / static_cast<float>(screenSize.x) : 0.0f,
                                          screenSize.y > 0 ? 1.0f / static_cast<float>(screenSize.y) : 0.0f);
        m.gaussianRenderingMode = 0.0f;
        _matrices->dirty();
    }

    void GaussianGeometry::sort(const vsg::dmat4& modelMatrix, const vsg::dmat4& viewMatrix)
    {
        if (!_ready || _numSplats == 0) return;

        vsg::dmat4 localToEye = viewMatrix * modelMatrix;
        std::vector<std::pair<double, std::uint32_t>> keys;
        keys.reserve(_numSplats);
        for (std::uint32_t i = 0; i < _numSplats; ++i)
        {
            const auto& p = (*_positions)[i];
            vsg::dvec4 v = localToEye * vsg::dvec4(p.x, p.y, p.z, 1.0);
            keys.emplace_back(v.z, i); // smaller view-space z means farther away
        }

        // ascending sort => back-to-front
        std::sort(keys.begin(), keys.end(), [](const auto& a, const auto& b) { return a.first < b.first; });
        for (std::uint32_t i = 0; i < _numSplats; ++i) (*_indices)[i] = keys[i].second;
        _indices->dirty();
    }

    // ------------------------------------------------------------------------
    // format loaders
    // ------------------------------------------------------------------------
    vsg::ref_ptr<GaussianGeometry> loadSplatFile(const std::string& filename)
    {
        std::vector<char> buffer;
        if (!readFileBinary(filename, buffer) || buffer.empty()) return {};

        struct SplatRecord
        {
            float position[3];
            float scale[3];
            std::uint8_t rgba[4];
            std::uint8_t rotation[4];
        };

        size_t numSplats = buffer.size() / sizeof(SplatRecord);
        if (numSplats == 0) return {};

        const float kSH_C0 = 0.28209479177387814f;
        auto positions = vsg::vec3Array::create(static_cast<uint32_t>(numSplats));
        auto scales = vsg::vec3Array::create(static_cast<uint32_t>(numSplats));
        auto rotations = vsg::vec4Array::create(static_cast<uint32_t>(numSplats));
        auto alphas = vsg::floatArray::create(static_cast<uint32_t>(numSplats));
        auto rD0 = vsg::vec4Array::create(static_cast<uint32_t>(numSplats));
        auto gD0 = vsg::vec4Array::create(static_cast<uint32_t>(numSplats));
        auto bD0 = vsg::vec4Array::create(static_cast<uint32_t>(numSplats));

        const auto* records = reinterpret_cast<const SplatRecord*>(buffer.data());
        for (size_t i = 0; i < numSplats; ++i)
        {
            const auto& r = records[i];
            (*positions)[i] = vsg::vec3(r.position[0], r.position[1], r.position[2]);
            (*scales)[i] = vsg::vec3(r.scale[0], r.scale[1], r.scale[2]);

            // rotation: file order is (w, x, y, z), stored as (x, y, z, w)
            vsg::vec4 rotValue((r.rotation[0] / 255.0f) * 2.0f - 1.0f,
                               (r.rotation[1] / 255.0f) * 2.0f - 1.0f,
                               (r.rotation[2] / 255.0f) * 2.0f - 1.0f,
                               (r.rotation[3] / 255.0f) * 2.0f - 1.0f);
            float len = std::sqrt(rotValue.x * rotValue.x + rotValue.y * rotValue.y + rotValue.z * rotValue.z + rotValue.w * rotValue.w);
            if (len > 0.0f) rotValue /= len;
            (*rotations)[i] = vsg::vec4(rotValue.y, rotValue.z, rotValue.w, rotValue.x);

            (*alphas)[i] = r.rgba[3] / 255.0f;
            (*rD0)[i] = vsg::vec4((r.rgba[0] / 255.0f - 0.5f) / kSH_C0, 0.0f, 0.0f, 0.0f);
            (*gD0)[i] = vsg::vec4((r.rgba[1] / 255.0f - 0.5f) / kSH_C0, 0.0f, 0.0f, 0.0f);
            (*bD0)[i] = vsg::vec4((r.rgba[2] / 255.0f - 0.5f) / kSH_C0, 0.0f, 0.0f, 0.0f);
        }

        auto geometry = GaussianGeometry::create();
        geometry->setShDegrees(0);
        geometry->setPosition(positions);
        geometry->setScaleAndRotation(scales, rotations);
        geometry->setAlpha(alphas);
        geometry->setShRed(0, rD0);
        geometry->setShGreen(0, gD0);
        geometry->setShBlue(0, bD0);
        geometry->finalize();
        return geometry;
    }

    vsg::ref_ptr<GaussianGeometry> loadPlyFile(const std::string& filename)
    {
        std::vector<char> buffer;
        if (!readFileBinary(filename, buffer) || buffer.empty()) return {};

        // parse PLY header
        std::string header;
        size_t pos = 0;
        auto readLine = [&](std::string& line) -> bool {
            if (pos >= buffer.size()) return false;
            size_t eol = pos;
            while (eol < buffer.size() && buffer[eol] != '\n') ++eol;
            line.assign(buffer.data() + pos, eol - pos);
            if (!line.empty() && line.back() == '\r') line.pop_back();
            pos = (eol < buffer.size()) ? eol + 1 : eol;
            return true;
        };

        std::string line;
        readLine(line); // "ply"
        if (line.find("ply") == std::string::npos) return {};

        bool binaryLittleEndian = false;
        std::vector<std::string> properties;
        uint32_t numSplats = 0;
        while (readLine(line))
        {
            if (line.find("format") == 0)
            {
                binaryLittleEndian = line.find("binary_little_endian") != std::string::npos;
            }
            else if (line.find("element vertex") == 0)
            {
                numSplats = static_cast<uint32_t>(std::strtoul(line.c_str() + 15, nullptr, 10));
            }
            else if (line.find("property float") == 0 || line.find("property double") == 0)
            {
                size_t sp = line.find_last_of(' ');
                properties.push_back(line.substr(sp + 1));
            }
            else if (line.find("end_header") == 0)
            {
                break;
            }
        }
        if (!binaryLittleEndian || numSplats == 0 || properties.empty()) return {};

        auto findIndex = [&](const std::string& name) -> int {
            for (size_t i = 0; i < properties.size(); ++i) if (properties[i] == name) return static_cast<int>(i);
            return -1;
        };

        const int idxX = findIndex("x"), idxY = findIndex("y"), idxZ = findIndex("z");
        const int idxS0 = findIndex("scale_0"), idxS1 = findIndex("scale_1"), idxS2 = findIndex("scale_2");
        const int idxO = findIndex("opacity");
        const int idxDC0 = findIndex("f_dc_0"), idxDC1 = findIndex("f_dc_1"), idxDC2 = findIndex("f_dc_2");
        const int idxR0 = findIndex("rot_0"), idxR1 = findIndex("rot_1"), idxR2 = findIndex("rot_2"), idxR3 = findIndex("rot_3");
        const int idxN0 = findIndex("nx"), idxN1 = findIndex("ny"), idxN2 = findIndex("nz");
        if (idxX < 0 || idxY < 0 || idxZ < 0 || idxS0 < 0 || idxS1 < 0 || idxS2 < 0 || idxO < 0) return {};

        bool hasRotation = idxR0 >= 0 && idxR1 >= 0 && idxR2 >= 0 && idxR3 >= 0;
        bool hasNormalRotation = idxN0 >= 0 && idxN1 >= 0 && idxN2 >= 0;
        bool hasSH = idxDC0 >= 0 && idxDC1 >= 0 && idxDC2 >= 0;

        const size_t elementSize = sizeof(float) * properties.size();
        if (pos + elementSize * numSplats > buffer.size()) return {};

        auto positions = vsg::vec3Array::create(numSplats);
        auto scales = vsg::vec3Array::create(numSplats);
        auto rotations = vsg::vec4Array::create(numSplats);
        auto alphas = vsg::floatArray::create(numSplats);
        auto rD0 = vsg::vec4Array::create(numSplats);
        auto gD0 = vsg::vec4Array::create(numSplats);
        auto bD0 = vsg::vec4Array::create(numSplats);

        // SH 1..15 coefficients (grouped into vec4 sets of 4 SH terms)
        auto shRed = std::vector<vsg::ref_ptr<vsg::vec4Array>>(4);
        auto shGreen = std::vector<vsg::ref_ptr<vsg::vec4Array>>(4);
        auto shBlue = std::vector<vsg::ref_ptr<vsg::vec4Array>>(4);
        for (int g = 0; g < 4; ++g)
        {
            shRed[g] = vsg::vec4Array::create(numSplats);
            shGreen[g] = vsg::vec4Array::create(numSplats);
            shBlue[g] = vsg::vec4Array::create(numSplats);
        }

        const char* data = buffer.data() + pos;
        std::vector<float> row(properties.size());
        const float kSH_C0 = 0.28209479177387814f;

        for (uint32_t i = 0; i < numSplats; ++i)
        {
            const float* f = reinterpret_cast<const float*>(data + i * elementSize);
            for (size_t p = 0; p < properties.size(); ++p) row[p] = f[p];

            (*positions)[i] = vsg::vec3(row[idxX], row[idxY], row[idxZ]);
            (*scales)[i] = vsg::vec3(std::exp(row[idxS0]), std::exp(row[idxS1]), std::exp(row[idxS2]));
            (*alphas)[i] = 1.0f / (1.0f + std::exp(-row[idxO])); // sigmoid

            if (hasRotation)
            {
                // rot_0..3 = (w, x, y, z), stored as (x, y, z, w)
                vsg::vec4 q(row[idxR0], row[idxR1], row[idxR2], row[idxR3]);
                float len = std::sqrt(q.x * q.x + q.y * q.y + q.z * q.z + q.w * q.w);
                if (len > 0.0f) q /= len;
                (*rotations)[i] = vsg::vec4(q.y, q.z, q.w, q.x);
            }
            else if (hasNormalRotation)
            {
                // standard 3DGS output: nx,ny,nz are the first 3 quaternion components, w is implied (>=0)
                float nx = row[idxN0], ny = row[idxN1], nz = row[idxN2];
                float w = std::sqrt(std::max(0.0f, 1.0f - (nx * nx + ny * ny + nz * nz)));
                (*rotations)[i] = vsg::vec4(nx, ny, nz, w);
            }
            else
            {
                (*rotations)[i] = vsg::vec4(0.0f, 0.0f, 0.0f, 1.0f);
            }

            if (hasSH)
            {
                (*rD0)[i] = vsg::vec4(row[idxDC0], 0.0f, 0.0f, 0.0f);
                (*gD0)[i] = vsg::vec4(row[idxDC1], 0.0f, 0.0f, 0.0f);
                (*bD0)[i] = vsg::vec4(row[idxDC2], 0.0f, 0.0f, 0.0f);
            }
            else
            {
                (*rD0)[i] = vsg::vec4(0.5f / kSH_C0, 0.0f, 0.0f, 0.0f);
                (*gD0)[i] = vsg::vec4(0.5f / kSH_C0, 0.0f, 0.0f, 0.0f);
                (*bD0)[i] = vsg::vec4(0.5f / kSH_C0, 0.0f, 0.0f, 0.0f);
            }

            // f_rest_0..44 = (r,g,b) of SH terms 1..15, 3 coefficients per term
            for (int g = 0; g < 4; ++g)
            {
                vsg::vec4 rv, gv, bv;
                for (int k = 0; k < 4; ++k)
                {
                    int shIndex = g * 4 + k; // SH term 1..15 (0 is DC, already in baseColor)
                    if (shIndex <= 0 || shIndex > 15) continue;
                    int restIndex = (shIndex - 1) * 3; // offset into f_rest
                    int ir = findIndex("f_rest_" + std::to_string(restIndex));
                    int ig = findIndex("f_rest_" + std::to_string(restIndex + 1));
                    int ib = findIndex("f_rest_" + std::to_string(restIndex + 2));
                    if (ir >= 0) rv[k] = row[ir];
                    if (ig >= 0) gv[k] = row[ig];
                    if (ib >= 0) bv[k] = row[ib];
                }
                (*shRed[g])[i] = rv;
                (*shGreen[g])[i] = gv;
                (*shBlue[g])[i] = bv;
            }
        }

        auto geometry = GaussianGeometry::create();
        geometry->setShDegrees(hasSH ? 3 : 0);
        geometry->setPosition(positions);
        geometry->setScaleAndRotation(scales, rotations);
        geometry->setAlpha(alphas);
        geometry->setShRed(0, rD0);
        geometry->setShGreen(0, gD0);
        geometry->setShBlue(0, bD0);
        if (hasSH)
        {
            for (int g = 0; g < 4; ++g)
            {
                geometry->setShRed(g, shRed[g]);
                geometry->setShGreen(g, shGreen[g]);
                geometry->setShBlue(g, shBlue[g]);
            }
        }
        geometry->finalize();
        return geometry;
    }
} // namespace vsg3dgs
