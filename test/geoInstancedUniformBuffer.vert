#version 410 core

layout (std140) uniform offsetInfo {
  vec4 offsets[100];
};

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
// in vec3 aInstanceOffset;

out vec3 vNormal;

void main( void ) {
  vec4 offset = offsets[gl_InstanceID];
  
  gl_Position = uMVP * vec4(aPosition, 1.0) + offset;

  vNormal = aNormal;
}
