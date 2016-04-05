#version 330 core

uniform mat4 uProjectionView;

in vec3 aPosition;
in vec3 aNormal;
in mat4 aInstanceTransform;
in vec4 aInstanceColor;

out vec3 vNormal;
out vec4 vColor;

void main( void ) { 

  gl_Position = uProjectionView * transpose(aInstanceTransform) * vec4(aPosition, 1.0);

  vNormal = aNormal;
  vColor = aInstanceColor;
}
