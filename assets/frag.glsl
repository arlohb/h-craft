#version 330

in vec2 fTexCoord;
in vec4 fCol;
in vec3 fPos;
in vec3 fNormal;

out vec4 col;

uniform float time;

void main() {
    float a = abs(sin(time));
    col = vec4(a, a, a, 1.0);
}

