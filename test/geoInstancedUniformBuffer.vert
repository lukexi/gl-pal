#version 410 core

struct Block {
  vec4 offset;
  vec4 color;
};

layout (std140) uniform myUniformBlock {
  Block blocks[100];
};

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
// in vec3 aInstanceOffset;

out vec3 vNormal;
out vec4 vColor;

void main() {
  Block block = blocks[gl_InstanceID];


  vec4 offset  = block.offset;
  vec4 color  = block.color;
  
  gl_Position = uMVP * vec4(aPosition, 1.0) + offset;

  vNormal = aNormal;
  vColor = color;
}
