#define TINYGLTF_IMPLEMENTATION
//#define TINYGLTF_ENABLE_DRACO
#define STB_IMAGE_STATIC
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "deps/tiny_gltf.h"

#include <vsg/all.h>
#include "gltf.h"
using namespace vsgRecipes;

extern vsg::ref_ptr<vsg::Object> loadGltfScene(tinygltf::Model& modelDef,
                                               vsg::ref_ptr<const vsg::Options> opt);
gltf::gltf() : _supportedExtensions{ ".gltf", ".glb" }{}
gltf::~gltf() {}

vsg::ref_ptr<vsg::Object> gltf::read(const vsg::Path& path, vsg::ref_ptr<const vsg::Options> opt) const
{
    tinygltf::Model modelDef;
    vsg::Path fileName = vsg::findFile(path, opt);
    if (!vsg::compatibleExtension(path, opt, _supportedExtensions)) return {};
    if (!fileName) return {};

    tinygltf::TinyGLTF loader; std::string err, warn;
    vsg::Path ext = vsg::fileExtension(fileName);
    if (ext == ".gltf")
    {
        bool loaded = loader.LoadASCIIFromFile(&modelDef, &err, &warn, fileName.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
    else //if (ext == ".glb")
    {
        bool loaded = loader.LoadBinaryFromFile(&modelDef, &err, &warn, fileName.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
}

vsg::ref_ptr<vsg::Object> gltf::read(std::istream& in, vsg::ref_ptr<const vsg::Options> opt) const
{
    std::istreambuf_iterator<char> eos;
    std::vector<char> data(std::istreambuf_iterator<char>(in), eos);
    unsigned int size = (unsigned int)data.size(); tinygltf::Model modelDef;
    if (!vsg::compatibleExtension(opt, _supportedExtensions) || !size) return {};

    tinygltf::TinyGLTF loader; std::string err, warn;
    vsg::Path ext = opt.valid() ? opt->extensionHint : "";
    vsg::Path dir = opt.valid() ? (opt->paths.empty() ? "." : opt->paths.back()) : ".";
    if (ext == ".gltf")
    {
        bool loaded = loader.LoadASCIIFromString(&modelDef, &err, &warn, &data[0], size, dir.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
    else //if (ext == ".glb")
    {
        bool loaded = loader.LoadBinaryFromMemory(&modelDef, &err, &warn, (uint8_t*)&data[0], size, dir.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
}

vsg::ref_ptr<vsg::Object> gltf::read(const uint8_t* ptr, size_t size,
                                     vsg::ref_ptr<const vsg::Options> opt) const
{
    tinygltf::Model modelDef;
    if (!vsg::compatibleExtension(opt, _supportedExtensions) || !size) return {};

    tinygltf::TinyGLTF loader; std::string err, warn;
    vsg::Path ext = opt.valid() ? opt->extensionHint : "";
    vsg::Path dir = opt.valid() ? (opt->paths.empty() ? "." : opt->paths.back()) : ".";
    if (ext == ".gltf")
    {
        bool loaded = loader.LoadASCIIFromString(&modelDef, &err, &warn, (char*)ptr, (unsigned int)size, dir.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
    else //if (ext == ".glb")
    {
        bool loaded = loader.LoadBinaryFromMemory(&modelDef, &err, &warn, ptr, (unsigned int)size, dir.string());
        return loaded ? loadGltfScene(modelDef, opt) : nullptr;
    }
}

bool gltf::getFeatures(Features& features) const
{
    vsg::ReaderWriter::FeatureMask rwMask = static_cast<vsg::ReaderWriter::FeatureMask>(
        /*WRITE_FILENAME | WRITE_OSTREAM |*/ READ_FILENAME | READ_ISTREAM | READ_MEMORY);
    features.extensionFeatureMap[".gltf"] = rwMask;
    features.extensionFeatureMap[".glb"] = rwMask;
    return true;
}

bool gltf::readOptions(vsg::Options& options, vsg::CommandLine& arguments) const
{
    return false;
}

/// Implementation class
class GltfImplementation
{
public:
    GltfImplementation(tinygltf::Model* model, vsg::ref_ptr<const vsg::Options> opt)
        : _model(model), _options(opt)
    {
        if (_options) _sharedObjects = _options->sharedObjects;
        if (!_sharedObjects) _sharedObjects = vsg::SharedObjects::create();
    }

    vsg::ref_ptr<vsg::Group> load(int sceneID)
    {
        const tinygltf::Scene& defScene = _model->scenes[sceneID];
        vsg::ref_ptr<vsg::Group> root = vsg::Group::create();
        for (size_t i = 0; i < defScene.nodes.size(); ++i)
        {
            vsg::ref_ptr<vsg::Node> node = loadNode(defScene.nodes[i]);
            if (node.valid()) root->addChild(node);
        }
        return root->children.empty() ? nullptr : root;
    }

    vsg::ref_ptr<vsg::Node> loadNode(int id)
    {
        tinygltf::Node& gltfNode = _model->nodes[id];
        bool emptyTRS = gltfNode.translation.empty() && gltfNode.rotation.empty() && gltfNode.scale.empty();
        bool emptyM = gltfNode.matrix.empty();

        vsg::ref_ptr<vsg::Group> group;
        if (!emptyTRS)
        {
            vsg::dmat4 scale, rot, trans;
            if (gltfNode.scale.size() == 3)
                scale = vsg::scale(gltfNode.scale[0], gltfNode.scale[1], gltfNode.scale[2]);
            if (gltfNode.rotation.size() == 4)
                rot = vsg::rotate(vsg::dquat(gltfNode.rotation[0], gltfNode.rotation[1],
                                             gltfNode.rotation[2], gltfNode.rotation[3]));
            if (gltfNode.translation.size() == 3)
                trans = vsg::translate(gltfNode.translation[0], gltfNode.translation[1], gltfNode.translation[2]);
            group = vsg::MatrixTransform::create(scale * rot * trans);
        }
        else if (!emptyM)
            group = vsg::MatrixTransform::create(vsg::dmat4(&gltfNode.matrix[0]));
        else
            group = vsg::Group::create();

        for (size_t i = 0; i < gltfNode.children.size(); ++i)
        {
            vsg::ref_ptr<vsg::Node> node = loadNode(gltfNode.children[i]);
            if (node.valid()) group->addChild(node);
        }

        if (gltfNode.mesh >= 0)
        {
            vsg::ref_ptr<vsg::Node> mesh = loadMeshData(gltfNode.mesh, gltfNode.skin);
            if (mesh.valid()) group->addChild(mesh);
        }
        group->setValue("name", gltfNode.name);
        return group;
    }

    vsg::ref_ptr<vsg::Node> loadMeshData(int meshID, int skinID)
    {
        tinygltf::Mesh& gltfMesh = _model->meshes[meshID];
        if (_meshes.find(meshID) != _meshes.end()) return _meshes[meshID];

        vsg::ref_ptr<vsg::Group> meshRoot = vsg::Group::create();
        meshRoot->setValue("name", gltfMesh.name);
        for (size_t i = 0; i < gltfMesh.primitives.size(); ++i)
        {
            tinygltf::Primitive gltfPrimitive = gltfMesh.primitives[i];
            vsg::ref_ptr<vsg::DescriptorConfigurator> material = loadMaterial(gltfPrimitive.material);

            vsg::DataList vertexArrayData;
            vsg::ref_ptr<vsg::GraphicsPipelineConfigurator> config =
                vsg::GraphicsPipelineConfigurator::create(material->shaderSet);
            config->descriptorConfigurator = material;
            if (_options.valid()) config->assignInheritedState(_options->inheritedState);

            bool hasNormals = false, hasColors = false, hasUVs = false;
            for (std::map<std::string, int>::iterator attrib = gltfPrimitive.attributes.begin();
                 attrib != gltfPrimitive.attributes.end(); ++attrib)
            {
                tinygltf::Accessor& attrAccessor = _model->accessors[attrib->second];
                const tinygltf::BufferView& attrView = _model->bufferViews[attrAccessor.bufferView];
                int size = (int)attrAccessor.count; if (!size || attrView.buffer < 0) continue;

                const tinygltf::Buffer& gltfBuffer = _model->buffers[attrView.buffer];
                int compNum = (attrAccessor.type != TINYGLTF_TYPE_SCALAR) ? attrAccessor.type : 1;
                int compSize = tinygltf::GetComponentSizeInBytes(attrAccessor.componentType);
                int copySize = size * (compSize * compNum);
                size_t offset = attrView.byteOffset + attrAccessor.byteOffset;
                size_t stride = (attrView.byteStride > 0 && attrView.byteStride != (compSize * compNum))
                              ? attrView.byteStride : 0;

                if (attrib->first.compare("POSITION") == 0 && compSize == 4 && compNum == 3)
                {
                    vsg::ref_ptr<vsg::vec3Array> va = vsg::vec3Array::create(size);
                    copyBufferData(va->dataPointer(), &gltfBuffer.data[offset], copySize, stride, size);
                    config->assignArray(vertexArrayData, "vsg_Vertex", VK_VERTEX_INPUT_RATE_VERTEX, va);
                }
                else if (attrib->first.compare("NORMAL") == 0 && compSize == 4 && compNum == 3)
                {
                    vsg::ref_ptr<vsg::vec3Array> na = vsg::vec3Array::create(size);
                    copyBufferData(na->dataPointer(), &gltfBuffer.data[offset], copySize, stride, size);
                    config->assignArray(vertexArrayData, "vsg_Normal", VK_VERTEX_INPUT_RATE_VERTEX, na);
                    hasNormals = true;
                }
                else if (attrib->first.compare("COLOR") == 0 && compSize == 4 && compNum == 4)
                {
                    vsg::ref_ptr<vsg::vec4Array> ca = vsg::vec4Array::create(size);
                    copyBufferData(ca->dataPointer(), &gltfBuffer.data[offset], copySize, stride, size);
                    config->assignArray(vertexArrayData, "vsg_Color", VK_VERTEX_INPUT_RATE_VERTEX, ca);
                    hasColors = true;
                }
                else if (attrib->first.find("TEXCOORD_") != std::string::npos && compSize == 4 && compNum == 2)
                {
                    int unit = atoi(attrib->first.substr(9).c_str());
                    if (unit == 0)
                    {
                        vsg::ref_ptr<vsg::vec2Array> ta = vsg::vec2Array::create(size);
                        copyBufferData(ta->dataPointer(), &gltfBuffer.data[offset], copySize, stride, size);
                        config->assignArray(vertexArrayData, "vsg_TexCoord0", VK_VERTEX_INPUT_RATE_VERTEX, ta);
                        hasUVs = true;
                    }
                    else
                        vsg::warn("[loadMeshData] Not implemented: texture coordinates with unit > 0");
                }
                else if (attrib->first.find("JOINTS_") != std::string::npos && compNum == 4)
                {
                    int jointID = atoi(attrib->first.substr(7).c_str());
                    // TODO
                    vsg::warn("[loadMeshData] Not implemented: read joints");
                }
                else if (attrib->first.find("WEIGHTS_") != std::string::npos && compSize == 4 && compNum == 4)
                {
                    int weightID = atoi(attrib->first.substr(8).c_str());
                    // TODO
                    vsg::warn("[loadMeshData] Not implemented: read weights");
                }
            }

            // Fallback for missing attributes
            if (!hasNormals)
            {
                vsg::ref_ptr<vsg::vec3Value> na = vsg::vec3Value::create(vsg::vec3(0.0f, 0.0f, 1.0f));
                config->assignArray(vertexArrayData, "vsg_Normal", VK_VERTEX_INPUT_RATE_INSTANCE, na);
            }
            if (!hasColors)
            {
                vsg::ref_ptr<vsg::vec4Value> ca = vsg::vec4Value::create(vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f));
                config->assignArray(vertexArrayData, "vsg_Color", VK_VERTEX_INPUT_RATE_INSTANCE, ca);
            }
            if (!hasUVs)
            {
                vsg::ref_ptr<vsg::vec2Value> ta = vsg::vec2Value::create(vsg::vec2(0.0f, 0.0f));
                config->assignArray(vertexArrayData, "vsg_TexCoord0", VK_VERTEX_INPUT_RATE_INSTANCE, ta);
            }

            // Configure primitive index array
            tinygltf::Accessor indexAccessor = _model->accessors[gltfPrimitive.indices];
            const tinygltf::BufferView& indexView = _model->bufferViews[indexAccessor.bufferView];

            vsg::ref_ptr<vsg::Data> indices;
            if (indexView.target == 0)
            {
                // TODO
                vsg::warn("[loadMeshData] Not implemented: ignore index array");
            }
            else  // ELEMENT_ARRAY_BUFFER = 34963
            {
                const tinygltf::Buffer& indexBuffer = _model->buffers[indexView.buffer];
                int compSize = tinygltf::GetComponentSizeInBytes(indexAccessor.componentType);
                int size = (int)indexAccessor.count; if (!size) continue;
                size_t stride = (indexView.byteStride > 0 && indexView.byteStride != compSize)
                              ? indexView.byteStride : 0;

                size_t offset = indexView.byteOffset + indexAccessor.byteOffset;
                switch (compSize)
                {
                case 1:
                    {
                        vsg::ref_ptr<vsg::ubyteArray> de = vsg::ubyteArray::create(size);
                        copyBufferData(de->dataPointer(), &indexBuffer.data[offset],
                                       size * compSize, stride, size); indices = de;
                    }
                    break;
                case 2:
                    {
                        vsg::ref_ptr<vsg::ushortArray> de = vsg::ushortArray::create(size);
                        copyBufferData(de->dataPointer(), &indexBuffer.data[offset],
                                       size * compSize, stride, size); indices = de;
                    }
                    break;
                case 4:
                    {
                        vsg::ref_ptr<vsg::uintArray> de = vsg::uintArray::create(size);
                        copyBufferData(de->dataPointer(), &indexBuffer.data[offset],
                                       size * compSize, stride, size); indices = de;
                    }
                    break;
                default:
                    vsg::warn("[loadMeshData] Unsupported index component size");
                    break;
                }
            }

            VkPrimitiveTopology topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
            switch (gltfPrimitive.mode)
            {
            case TINYGLTF_MODE_POINTS:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_POINT_LIST; break;
            case TINYGLTF_MODE_LINE:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_LINE_LIST; break;
            case TINYGLTF_MODE_LINE_LOOP:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_LINE_STRIP; break;  // FIXME
            case TINYGLTF_MODE_LINE_STRIP:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_LINE_STRIP; break;
            case TINYGLTF_MODE_TRIANGLES:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST; break;
            case TINYGLTF_MODE_TRIANGLE_STRIP:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP; break;
            case TINYGLTF_MODE_TRIANGLE_FAN:
                topology = VkPrimitiveTopology::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN; break;
            default: break;
            }

            if (!indices) continue;
            vsg::ref_ptr<vsg::VertexIndexDraw> drawer = vsg::VertexIndexDraw::create();
            drawer->setValue("name", gltfMesh.name + "_" + std::to_string(i));
            drawer->assignArrays(vertexArrayData);
            drawer->assignIndices(indices);
            drawer->indexCount = static_cast<uint32_t>(indices->valueCount());
            drawer->instanceCount = 1;

            // set the GraphicsPipelineStates to the required values
            struct SetPipelineStates : public vsg::Visitor
            {
                VkPrimitiveTopology topology = VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST;
                bool blending = false, two_sided = false;
                SetPipelineStates(VkPrimitiveTopology in_topology, bool in_blending, bool in_two_sided)
                    : topology(in_topology), blending(in_blending), two_sided(in_two_sided) {}

                void apply(vsg::Object& object) { object.traverse(*this); }
                void apply(vsg::RasterizationState& rs) { if (two_sided) rs.cullMode = VK_CULL_MODE_NONE; }
                void apply(vsg::InputAssemblyState& ias) { ias.topology = topology; }
                void apply(vsg::ColorBlendState& cbs) { cbs.configureAttachments(blending); }
            } sps(topology, material->blending, material->two_sided);

            config->accept(sps);
            if (!_sharedObjects) config->init();
            else _sharedObjects->share(config, [](auto gpc) { gpc->init(); });

            // create StateGroup as the root of the scene/command graph to hold the GraphicsPipeline
            vsg::ref_ptr<vsg::StateGroup> stateGroup = vsg::StateGroup::create();
            config->copyTo(stateGroup, _sharedObjects);
            stateGroup->addChild(drawer);

            if (material->blending)
            {
                vsg::ComputeBounds computeBounds; drawer->accept(computeBounds);
                vsg::dvec3 center = (computeBounds.bounds.min + computeBounds.bounds.max) * 0.5;
                double radius = vsg::length(computeBounds.bounds.max - computeBounds.bounds.min) * 0.5;

                vsg::ref_ptr<vsg::DepthSorted> depthSorted = vsg::DepthSorted::create();
                depthSorted->binNumber = 10;
                depthSorted->bound.set(center[0], center[1], center[2], radius);
                depthSorted->child = stateGroup;
                meshRoot->addChild(depthSorted);
            }
            else
                meshRoot->addChild(stateGroup);
        }
        return (meshRoot->children.size() == 1) ? meshRoot->children[0] : meshRoot;
    }

    vsg::ref_ptr<vsg::DescriptorConfigurator> loadMaterial(int mtlID)
    {
        tinygltf::Material& gltfMtl = _model->materials[mtlID];
        if (_materials.find(mtlID) != _materials.end()) return _materials[mtlID];

        int baseID = gltfMtl.pbrMetallicRoughness.baseColorTexture.index;
        int metallicID = gltfMtl.pbrMetallicRoughness.metallicRoughnessTexture.index;
        int normalID = gltfMtl.normalTexture.index;
        int emissiveID = gltfMtl.emissiveTexture.index;
        int occlusionID = gltfMtl.occlusionTexture.index;

        vsg::ref_ptr<vsg::DescriptorConfigurator> material = vsg::DescriptorConfigurator::create();
        std::set<std::string>& defines = material->defines;
        material->blending = (gltfMtl.alphaMode.compare("BLEND") == 0);
        if (gltfMtl.doubleSided) { material->two_sided = true; defines.insert("VSG_TWO_SIDED_LIGHTING"); }

        if (metallicID >= 0 || occlusionID >= 0)
        {
            vsg::PbrMaterial pbr;
            if (material->blending) pbr.alphaMask = 0.0f;
            pbr.alphaMaskCutoff = (float)gltfMtl.alphaCutoff;
            pbr.metallicFactor = (float)gltfMtl.pbrMetallicRoughness.metallicFactor;
            pbr.roughnessFactor = (float)gltfMtl.pbrMetallicRoughness.roughnessFactor;
            if (gltfMtl.emissiveFactor.size() == 3) pbr.emissiveFactor = vsg::vec4(
                (float)gltfMtl.emissiveFactor[0], (float)gltfMtl.emissiveFactor[1], (float)gltfMtl.emissiveFactor[2], 1.0f);

            material->shaderSet = getOrCreatePbrShaderSet();
            if (baseID >= 0) loadTexture(baseID, "diffuseMap", material);
            if (metallicID >= 0) loadTexture(baseID, "mrMap", material);
            if (normalID >= 0) loadTexture(baseID, "normalMap", material);
            if (emissiveID >= 0) loadTexture(baseID, "emissiveMap", material);
            if (occlusionID >= 0) loadTexture(baseID, "aoMap", material);
            //material->assignTexture("specularMap");
            material->assignDescriptor("material", vsg::PbrMaterialValue::create(pbr));
        }
        else
        {
            vsg::PhongMaterial phong;
            if (material->blending) phong.alphaMask = 0.0f;
            phong.alphaMaskCutoff = (float)gltfMtl.alphaCutoff;
            if (gltfMtl.pbrMetallicRoughness.baseColorFactor.size() == 4) phong.ambient = vsg::vec4(
                (float)gltfMtl.pbrMetallicRoughness.baseColorFactor[0], (float)gltfMtl.pbrMetallicRoughness.baseColorFactor[1],
                (float)gltfMtl.pbrMetallicRoughness.baseColorFactor[2], (float)gltfMtl.pbrMetallicRoughness.baseColorFactor[3]);
            phong.diffuse = vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f);
            phong.specular = vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f);
            phong.emissive = vsg::vec4(0.0f, 0.0f, 0.0f, 1.0f);
            phong.shininess = 0.0f;

            material->shaderSet = getOrCreatePhongShaderSet();
            if (baseID >= 0) loadTexture(baseID, "diffuseMap", material);
            if (normalID >= 0) loadTexture(baseID, "normalMap", material);
            if (emissiveID >= 0) loadTexture(baseID, "emissiveMap", material);
            if (occlusionID >= 0) loadTexture(baseID, "aoMap", material);
            //material->assignTexture("specularMap");
            material->assignDescriptor("material", vsg::PhongMaterialValue::create(phong));
        }
        
        if (_sharedObjects)
        {
            for (auto& ds : material->descriptorSets)
            {
                if (ds) { _sharedObjects->share(ds->descriptors); _sharedObjects->share(ds); }
            }
        }
        _materials[mtlID] = material;
        return material;
    }

    void loadTexture(int id, const std::string& name, vsg::ref_ptr<vsg::DescriptorConfigurator> material)
    {
        tinygltf::Texture& gltfTex = _model->textures[id];
        tinygltf::Image& gltfImage = _model->images[gltfTex.source];
        tinygltf::Sampler& gltfSampler = _model->samplers[gltfTex.sampler];
        if (gltfImage.image.empty()) return;

        vsg::ref_ptr<vsg::Data> data;
        if (gltfImage.width < 1 || gltfImage.height < 1)
        {
            std::stringstream dataIn(std::ios::in | std::ios::out | std::ios::binary);
            dataIn.write((char*)&gltfImage.image[0], gltfImage.image.size());
            if (gltfImage.mimeType.find("ktx") != std::string::npos)
            {
                vsg::ref_ptr<vsg::Options> opt = vsg::Options::create();
                opt->extensionHint = vsg::Path(".ktx");
                data = vsg::read_cast<vsg::Data>(dataIn, opt);
            }
        }
        else
        {
            if (gltfImage.bits == 16)
            {
                // TODO
                vsg::warn("[loadTexture] Not implemented: 16bit image");
            }
            else
            {
                if (gltfImage.component == 4)
                {
                    vsg::ref_ptr<vsg::ubvec4Array2D> image = vsg::ubvec4Array2D::create(
                        gltfImage.width, gltfImage.height, vsg::Data::Properties{ VK_FORMAT_R8G8B8A8_UNORM });
                    memcpy(image->dataPointer(), &gltfImage.image[0], gltfImage.image.size()); data = image;
                }
                else if (gltfImage.component == 2)
                {
                    vsg::ref_ptr<vsg::ubvec2Array2D> image = vsg::ubvec2Array2D::create(
                        gltfImage.width, gltfImage.height, vsg::Data::Properties{ VK_FORMAT_R8G8_UNORM });
                    memcpy(image->dataPointer(), &gltfImage.image[0], gltfImage.image.size()); data = image;
                }
                else if (gltfImage.component == 1)
                {
                    vsg::ref_ptr<vsg::ubyteArray2D> image = vsg::ubyteArray2D::create(
                        gltfImage.width, gltfImage.height, vsg::Data::Properties{ VK_FORMAT_R8_UNORM });
                    memcpy(image->dataPointer(), &gltfImage.image[0], gltfImage.image.size()); data = image;
                }
            }
        }

        if (!data.valid()) return;
        if (name == "diffuseMap" || name == "emissiveMap")
        {
            switch (data->properties.format)
            {
            case VK_FORMAT_R8G8B8A8_UNORM: data->properties.format = VK_FORMAT_R8G8B8A8_SRGB; break;
            case VK_FORMAT_R8_UNORM: data->properties.format = VK_FORMAT_R8_SRGB; break;
            case VK_FORMAT_R8G8_UNORM: data->properties.format = VK_FORMAT_R8G8_SRGB; break;
            default: break;
            }
        }

        vsg::ref_ptr<vsg::Sampler> sampler = vsg::Sampler::create();
        sampler->addressModeU = getWrapMode(gltfSampler.wrapS);
        sampler->addressModeV = getWrapMode(gltfSampler.wrapT);
        sampler->addressModeW = VK_SAMPLER_ADDRESS_MODE_REPEAT;
        sampler->magFilter = getFilterMode(gltfSampler.magFilter, sampler->mipmapMode);
        sampler->minFilter = getFilterMode(gltfSampler.minFilter, sampler->mipmapMode);
        sampler->anisotropyEnable = VK_TRUE;
        sampler->maxAnisotropy = 16.0f;
        sampler->maxLod = data->properties.maxNumMipmaps;

        if (sampler->maxLod <= 1.0)
        {
            // Calculate maximum lod level
            auto maxDim = std::max(data->width(), data->height());
            sampler->maxLod = std::floor(std::log2f(static_cast<float>(maxDim)));
        }

        if (_sharedObjects) { _sharedObjects->share(data); _sharedObjects->share(sampler); }
        material->assignTexture(name, data, sampler);
    }

    VkFilter getFilterMode(int filter, VkSamplerMipmapMode& mipmap) const
    {
        switch (filter)
        {
        case 0: return VK_FILTER_NEAREST;  // NEAREST
        case 1: return VK_FILTER_LINEAR;  // LINEAR
        case 2: mipmap = VK_SAMPLER_MIPMAP_MODE_NEAREST; return VK_FILTER_NEAREST;  // NEAREST_MIPMAP_NEAREST
        case 3: mipmap = VK_SAMPLER_MIPMAP_MODE_NEAREST; return VK_FILTER_LINEAR;  // LINEAR_MIPMAP_NEAREST
        case 4: mipmap = VK_SAMPLER_MIPMAP_MODE_LINEAR; return VK_FILTER_NEAREST;  // NEAREST_MIPMAP_LINEAR
        case 5: mipmap = VK_SAMPLER_MIPMAP_MODE_LINEAR; return VK_FILTER_LINEAR;  // LINEAR_MIPMAP_LINEAR
        default: return VK_FILTER_NEAREST;
        }
    }

    VkSamplerAddressMode getWrapMode(int wrapMode) const
    {
        switch (wrapMode)
        {
        case TINYGLTF_TEXTURE_WRAP_REPEAT: return VK_SAMPLER_ADDRESS_MODE_REPEAT;
        case TINYGLTF_TEXTURE_WRAP_MIRRORED_REPEAT: return VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT;
        default: return VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
        }
    }

protected:
    inline void copyBufferData(void* dst, const void* src, size_t size, size_t stride, size_t count)
    {
        if (stride > 0 && count > 0)
        {
            size_t elemSize = size / count;
            for (size_t i = 0; i < count; ++i)
                memcpy((char*)dst + i * elemSize, (const char*)src + i * stride, elemSize);
        }
        else
            memcpy(dst, src, size);
    }

    vsg::ref_ptr<vsg::ShaderSet> getOrCreatePbrShaderSet()
    {
        if (!_pbrShaderSet)
        {
            _pbrShaderSet = vsg::createPhysicsBasedRenderingShaderSet(_options);
            if (_sharedObjects) _sharedObjects->share(_pbrShaderSet);
        }
        return _pbrShaderSet;
    }

    vsg::ref_ptr<vsg::ShaderSet> getOrCreatePhongShaderSet()
    {
        if (!_phongShaderSet)
        {
            _phongShaderSet = vsg::createPhongShaderSet(_options);
            if (_sharedObjects) _sharedObjects->share(_phongShaderSet);
        }
        return _phongShaderSet;
    }

    tinygltf::Model* _model;
    vsg::ref_ptr<const vsg::Options> _options;
    vsg::ref_ptr<vsg::ShaderSet> _pbrShaderSet;
    vsg::ref_ptr<vsg::ShaderSet> _phongShaderSet;
    vsg::ref_ptr<vsg::SharedObjects> _sharedObjects;
    std::map<int, vsg::ref_ptr<vsg::DescriptorConfigurator>> _materials;
    std::map<int, vsg::ref_ptr<vsg::Node>> _meshes;
};

vsg::ref_ptr<vsg::Object> loadGltfScene(tinygltf::Model& modelDef, vsg::ref_ptr<const vsg::Options> opt)
{
    GltfImplementation implementation(&modelDef, opt);
    return implementation.load(modelDef.defaultScene);
}
