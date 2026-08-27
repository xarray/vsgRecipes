#pragma once

#include <string>

// 3D Gaussian Splatting shaders, ported from the osgVerse (OSG) reference implementation
// (/gaussian_splatting.vert.glsl / gaussian_splatting.frag.glsl).
// USE_INSTANCING must be defined in the ShaderCompileSettings when compiling these shaders.

namespace vsg3dgs
{
    inline const std::string& getGaussianSplattingVertexShader()
    {
        static const std::string source = R"(
#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable
#pragma import_defines(USE_INSTANCING, FULL_SH)

struct ShcoefData
{
    uvec2 rgb0; uvec2 rgb1; uvec2 rgb2; uvec2 rgb3; uvec2 rgb4;
    uvec2 rgb5; uvec2 rgb6; uvec2 rgb7; uvec2 rgb8; uvec2 rgb9;
    uvec2 rgb10; uvec2 rgb11; uvec2 rgb12; uvec2 rgb13; uvec2 rgb14;
};

layout(std430, set = 0, binding = 0) restrict readonly buffer CorePosBuffer { vec4 corePos[]; };
layout(std430, set = 0, binding = 1) restrict readonly buffer CoreCov0Buffer { uvec2 coreCov0[]; };
layout(std430, set = 0, binding = 2) restrict readonly buffer CoreCov1Buffer { uvec2 coreCov1[]; };
layout(std430, set = 0, binding = 3) restrict readonly buffer CoreCov2Buffer { uvec2 coreCov2[]; };
layout(std430, set = 0, binding = 4) restrict readonly buffer ShcoefBuffer { ShcoefData shcoef[]; };

layout(std140, set = 0, binding = 5) uniform MatrixUBO
{
    mat4 modelMatrix;
    mat4 viewMatrix;
    mat4 projectionMatrix;
    vec2 nearFarPlanes;
    vec2 invScreenResolution;
    float gaussianRenderingMode;
} matrices;

layout(location = 0) in vec3 quadVertex;
layout(location = 1) in uint instanceIndex;

layout(location = 0) out vec4 color;
layout(location = 1) out vec4 invCovariance;
layout(location = 2) out vec2 center2D;

vec4 unpackHalf4(uvec2 v)
{
    vec2 lo = unpackHalf2x16(v.x);  // x, y
    vec2 hi = unpackHalf2x16(v.y);  // z, w
    return vec4(lo.x, lo.y, hi.x, hi.y);
}

mat3 computeRotationMat3(in vec4 q)
{
    float x = q.x, y = q.y, z = q.z, w = q.w;
    float x2 = x + x, y2 = y + y, z2 = z + z;
    float xx = x * x2, xy = x * y2, xz = x * z2;
    float yy = y * y2, yz = y * z2, zz = z * z2;
    float wx = w * x2, wy = w * y2, wz = w * z2;
    return mat3(1.0 - (yy + zz), xy + wz, xz - wy,
                xy - wz, 1.0 - (xx + zz), yz + wx,
                xz + wy, yz - wx, 1.0 - (xx + yy));
}

mat3 transposeMat3(in mat3 m)
{
    return mat3(m[0][0], m[1][0], m[2][0],
                m[0][1], m[1][1], m[2][1],
                m[0][2], m[1][2], m[2][2]);
}

mat3 computeCovariance(in vec3 scale, in vec4 quat)
{
    mat3 S2 = mat3(scale.x * scale.x, 0.0, 0.0,
                   0.0, scale.y * scale.y, 0.0,
                   0.0, 0.0, scale.z * scale.z);
    mat3 R = computeRotationMat3(quat);
    mat3 Rt = transposeMat3(R);
    return R * (S2 * Rt);
}

vec3 computeRadianceFromSH(in vec3 v, in vec3 baseColor)
{
    float b[16];
    float vx2 = v.x * v.x;
    float vy2 = v.y * v.y;
    float vz2 = v.z * v.z;

    float k1 = 0.4886025119029199;  // first order (/ (sqrt 3.0) (* 2 (sqrt pi)))
    b[0] = 0.28209479177387814;     // zeroth order (/ 1.0 (* 2.0 (sqrt pi)))
    b[1] = -k1 * v.y; b[2] = k1 * v.z; b[3] = -k1 * v.x;

#ifdef FULL_SH
    // second order
    float k2 = 1.0925484305920792;   // (/ (sqrt 15.0) (* 2 (sqrt pi)))
    float k3 = 0.31539156525252005;  // (/ (sqrt 5.0) (* 4 (sqrt  pi)))
    float k4 = 0.5462742152960396;   // (/ (sqrt 15.0) (* 4 (sqrt pi)))
    b[4] = k2 * v.y * v.x;
    b[5] = -k2 * v.y * v.z;
    b[6] = k3 * (3.0 * vz2 - 1.0);
    b[7] = -k2 * v.x * v.z;
    b[8] = k4 * (vx2 - vy2);

    // third order
    float k5 = 0.5900435899266435;  // (/ (* (sqrt 2) (sqrt 35)) (* 8 (sqrt pi)))
    float k6 = 2.8906114426405543;  // (/ (sqrt 105) (* 2 (sqrt pi)))
    float k7 = 0.4570457994644658;  // (/ (* (sqrt 2) (sqrt 21)) (* 8 (sqrt pi)))
    float k8 = 0.37317633259011546; // (/ (sqrt 7) (* 4 (sqrt pi)))
    float k9 = 1.4453057213202771;  // (/ (sqrt 105) (* 4 (sqrt pi)))
    b[9] = -k5 * v.y * (3.0 * vx2 - vy2);
    b[10] = k6 * v.y * v.x * v.z;
    b[11] = -k7 * v.y * (5.0 * vz2 - 1.0);
    b[12] = k8 * v.z * (5.0 * vz2 - 3.0);
    b[13] = -k7 * v.x * (5.0 * vz2 - 1.0);
    b[14] = k9 * v.z * (vx2 - vy2);
    b[15] = -k5 * v.x * (vx2 - 3.0 * vy2);

    ShcoefData shData = shcoef[instanceIndex];
    vec4 sh_rgb0 = unpackHalf4(shData.rgb0), sh_rgb1 = unpackHalf4(shData.rgb1),
         sh_rgb2 = unpackHalf4(shData.rgb2), sh_rgb3 = unpackHalf4(shData.rgb3), sh_rgb4 = unpackHalf4(shData.rgb4);
    vec4 sh_rgb5 = unpackHalf4(shData.rgb5), sh_rgb6 = unpackHalf4(shData.rgb6),
         sh_rgb7 = unpackHalf4(shData.rgb7), sh_rgb8 = unpackHalf4(shData.rgb8), sh_rgb9 = unpackHalf4(shData.rgb9);
    vec4 sh_rgb10 = unpackHalf4(shData.rgb10), sh_rgb11 = unpackHalf4(shData.rgb11),
         sh_rgb12 = unpackHalf4(shData.rgb12), sh_rgb13 = unpackHalf4(shData.rgb13), sh_rgb14 = unpackHalf4(shData.rgb14);

    float re = (b[0] * baseColor.x + b[1] * sh_rgb0.x + b[2] * sh_rgb1.x + b[3] * sh_rgb2.x +
                b[4] * sh_rgb3.x + b[5] * sh_rgb4.x + b[6] * sh_rgb5.x + b[7] * sh_rgb6.x +
                b[8] * sh_rgb7.x + b[9] * sh_rgb8.x + b[10] * sh_rgb9.x + b[11] * sh_rgb10.x +
                b[12] * sh_rgb11.x + b[13] * sh_rgb12.x + b[14] * sh_rgb13.x + b[15] * sh_rgb14.x);
    float gr = (b[0] * baseColor.y + b[1] * sh_rgb0.y + b[2] * sh_rgb1.y + b[3] * sh_rgb2.y +
                b[4] * sh_rgb3.y + b[5] * sh_rgb4.y + b[6] * sh_rgb5.y + b[7] * sh_rgb6.y +
                b[8] * sh_rgb7.y + b[9] * sh_rgb8.y + b[10] * sh_rgb9.y + b[11] * sh_rgb10.y +
                b[12] * sh_rgb11.y + b[13] * sh_rgb12.y + b[14] * sh_rgb13.y + b[15] * sh_rgb14.y);
    float bl = (b[0] * baseColor.z + b[1] * sh_rgb0.z + b[2] * sh_rgb1.z + b[3] * sh_rgb2.z +
                b[4] * sh_rgb3.z + b[5] * sh_rgb4.z + b[6] * sh_rgb5.z + b[7] * sh_rgb6.z +
                b[8] * sh_rgb7.z + b[9] * sh_rgb8.z + b[10] * sh_rgb9.z + b[11] * sh_rgb10.z +
                b[12] * sh_rgb11.z + b[13] * sh_rgb12.z + b[14] * sh_rgb13.z + b[15] * sh_rgb14.z);
#else
    float re = b[0] * baseColor.x, gr = b[0] * baseColor.y, bl = b[0] * baseColor.z;
#endif
    return vec3(0.5, 0.5, 0.5) + vec3(re, gr, bl);
}

mat2 inverseMat2(mat2 m)
{
    float det = m[0][0] * m[1][1] - m[0][1] * m[1][0];
    mat2 inv;
    inv[0][0] = m[1][1] / det; inv[0][1] = -m[0][1] / det;
    inv[1][0] = -m[1][0] / det; inv[1][1] = m[0][0] / det;
    return inv;
}

vec4 computeExtens2D(in float k, in mat2 cov2D, out vec4 cov2Dinv4)
{
    mat2 cov2Dinv = inverseMat2(cov2D);
    cov2Dinv4 = vec4(cov2Dinv[0], cov2Dinv[1]);

    // compute 2d extents for the splat, using covariance matrix ellipse (https://cookierobotics.com/007/)
    float a = cov2D[0][0], b = cov2D[0][1], c = cov2D[1][1];
    float apco2 = (a + c) / 2.0, amco2 = (a - c) / 2.0;
    float term = sqrt(amco2 * amco2 + b * b);
    float maj = apco2 + term, min = apco2 - term;

    float theta = (b == 0.0) ? ((a >= c) ? 0.0 : radians(90.0)) : atan(maj - a, b);
    float r1 = k * sqrt(maj), r2 = k * sqrt(min);
    vec2 majAxis = vec2(r1 * cos(theta), r1 * sin(theta));
    vec2 minAxis = vec2(r2 * cos(theta + radians(90.0)), r2 * sin(theta + radians(90.0)));
    return vec4(majAxis, minAxis);
}

mat3 computeJacobian(in mat4 projMat, in vec4 eyeVertex, in float invW, in float invH)
{
    // J is the jacobian of the projection and viewport transformations.
    // this is an affine approximation of the real projection.
    float FAR_NEAR = matrices.nearFarPlanes.y - matrices.nearFarPlanes.x, eyeZsq = eyeVertex.z * eyeVertex.z;
    float SX = projMat[0][0], SY = projMat[1][1], WZ = projMat[3][2];
    float jsx = -(SX * invW) / (2.0 * eyeVertex.z);
    float jsy = -(SY * invH) / (2.0 * eyeVertex.z);
    float jtx = (SX * eyeVertex.x * invW) / (2.0 * eyeZsq);
    float jty = (SY * eyeVertex.y * invH) / (2.0 * eyeZsq);
    float jtz = (FAR_NEAR * WZ) / (2.0 * eyeZsq);
    return mat3(vec3(jsx, 0.0, 0.0), vec3(0.0, jsy, 0.0), vec3(jtx, jty, jtz));
}

void main()
{
#if defined(USE_INSTANCING)
    int index = int(instanceIndex);
    vec4 posAlpha = corePos[index];
    vec4 cov0 = unpackHalf4(coreCov0[index]), cov1 = unpackHalf4(coreCov1[index]),
         cov2 = unpackHalf4(coreCov2[index]);
#else
    int index = 0;
    vec4 posAlpha = vec4(quadVertex, 1.0);
    vec4 cov0 = vec4(1.0, 1.0, 1.0, 0.5), cov1 = vec4(0.0, 0.0, 0.0, 0.5), cov2 = vec4(1.0, 1.0, 1.0, 0.5);
#endif

    mat4 modelViewMatrix = matrices.viewMatrix * matrices.modelMatrix;
    vec4 eyeVertex = modelViewMatrix * vec4(posAlpha.xyz, 1.0);
    vec3 baseColor = vec3(cov0.w, cov1.w, cov2.w);
    float alpha = posAlpha.w;

    mat3 V = mat3(0.0001, 0.0, 0.0, 0.0, 0.0001, 0.0, 0.0, 0.0, 0.0001);
    if (matrices.gaussianRenderingMode < 0.5)
    {
#if defined(USE_INSTANCING)
        V = computeCovariance(cov0.xyz, vec4(cov1.xyz, cov2.x));
#else
        V = computeCovariance(vec3(1.0), vec4(0.0, 0.0, 0.0, 1.0));
#endif
    }

    // combine the affine transforms of W (viewMat) and J (approx of viewportMat * projMat)
    // using the fact that the new transformed covariance matrix V_Prime = JW * V * (JW)^T
    float WIDTH = 1.0 / matrices.invScreenResolution.x, HEIGHT = 1.0 / matrices.invScreenResolution.y;
    mat3 J = computeJacobian(matrices.projectionMatrix, eyeVertex, WIDTH, HEIGHT);
    mat3 W = mat3(modelViewMatrix); mat3 JW = J * W; mat3 V_prime = JW * V * transpose(JW);

    mat2 cov2D = mat2(V_prime);  // 'project' the 3D covariance matrix onto xy plane
    float X0 = 0.0, Y0 = 0.0;  // viewport X & Y... FIXME: always 0?
    vec4 proj = matrices.projectionMatrix * eyeVertex;
    cov2D[0][0] += 0.3; cov2D[1][1] += 0.3;  // The convolution of a gaussian with another is the sum of their
                                             // covariance matrices, apply a low-pass filter for antialiasing

    vec4 covariance = vec4(cov2D[0], cov2D[1]);
    center2D = vec2(proj.x / proj.w, proj.y / proj.w);
    center2D.x = 0.5 * (WIDTH + (center2D.x * WIDTH) + (2.0 * X0));
    center2D.y = 0.5 * (HEIGHT + (center2D.y * HEIGHT) + (2.0 * Y0));

    // compute radiance from SH
    vec3 eyeDirection = normalize(eyeVertex.xyz / eyeVertex.w);
    vec3 ndcP = proj.xyz / proj.w;
    // VSG uses reverse-z, NDC z range is [0, 1]
    if (!(ndcP.z < 0.0 || ndcP.z > 1.0 || ndcP.x < -1.0 ||
          ndcP.x > 1.0 || ndcP.y < -1.0 || ndcP.y > 1.0))
    {
        float peakOpacity = alpha, minVisibleAlpha = 1.0 / 255.0;
        float maxDist2 = -2.0 * log(minVisibleAlpha / peakOpacity);
        if (maxDist2 < 0.0) { gl_Position = vec4(0.0, 0.0, -10.0, 1.0); return; }

        float k = min(sqrt(maxDist2), 3.0);  // use dynamic quad-size
        vec4 axes = computeExtens2D(k, cov2D, invCovariance);
        vec2 majAxis = axes.xy, minAxis = axes.zw;

        float projArea = length(majAxis) * length(minAxis);
        if (projArea < 1.0 || (projArea < 4.0 && alpha < 0.5))
        { gl_Position = vec4(0.0, 0.0, -10.0, 1.0); return; }

        vec2 offset = majAxis * quadVertex.x + minAxis * quadVertex.y;
        offset.x *= (2.0 * matrices.invScreenResolution.x) * proj.w;
        offset.y *= (2.0 * matrices.invScreenResolution.y) * proj.w; proj.xy += offset;
    }
    else
        { gl_Position = vec4(0.0, 0.0, -10.0, 1.0); return; }

    color = vec4(computeRadianceFromSH(eyeDirection, baseColor), alpha);
    gl_Position = proj;
}
)";
        return source;
    }

    inline const std::string& getGaussianSplattingFragmentShader()
    {
        static const std::string source = R"(
#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_ARB_shading_language_420pack : enable

layout(location = 0) in vec4 color;
layout(location = 1) in vec4 invCovariance;
layout(location = 2) in vec2 center2D;
layout(location = 0) out vec4 fragData;

void main()
{
    vec2 d = gl_FragCoord.xy - center2D;
    mat2 cov2Dinv = mat2(invCovariance.xy, invCovariance.zw);
    float g = exp(-0.5 * dot(d, cov2Dinv * d));

    float alpha = color.a * g; if (alpha <= 0.01) discard;
    alpha = min(alpha, 0.99); fragData = vec4(color.rgb * alpha, alpha);
}
)";
        return source;
    }
} // namespace vsg3dgs
