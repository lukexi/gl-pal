#version 330 core

in vec3 vNormal;
in vec2 vUV;
out vec4 color;

void main(void) {

  color = vec4(abs(vNormal), 1.0);
  // color = vec4(vUV, 1,1);
}
