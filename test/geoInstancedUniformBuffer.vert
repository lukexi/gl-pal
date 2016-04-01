#version 410 core

struct Block {
    vec4 position;
    vec4 color;
};

layout (std140) uniform myUniformBlock {
    Block blocks[1000];
};

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
// in vec3 aInstanceOffset;

out vec3 vNormal;
out vec4 vColor;

void main() {
    Block block = blocks[gl_InstanceID];
  
  
    vec4 position  = block.position;
    vec4 color  = block.color;
    
    gl_Position = uMVP * vec4(aPosition, 1.0) + position;
  
    vNormal = aNormal;
    vColor = color;
}
