#version 330

in vec2 fragTexCoord;
in vec4 fragColor;
in vec3 fragPosition;
in vec3 fragNormal;

out vec4 col;

uniform float time;

void main() {
    float a = abs(sin(time));
    // col = vec4(a, a, a, 1.0);
    col = vec4(fragNormal, 1.0);
}

