#version 330

in vec3 vPos;
in vec3 vNormal;
in vec2 vTexCoord;
in vec4 vCol;

out vec2 fTexCoord;
out vec4 fCol;
out vec3 fPos;
out vec3 fNormal;

uniform mat4 mModel;
uniform mat4 mNormal;
uniform mat4 mvp;

void main() {
    fPos = vec3(mModel * vec4(vPos, 1.0));
    fTexCoord = vTexCoord;
    fCol = vCol;
    fNormal = normalize(vec3(mNormal * vec4(vNormal, 1.0)));

    gl_Position = mvp * vec4(vPos, 1.0);
}

