#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
in vec3 aInstancePosition;
// in int aInstancePosI;

out vec3 vNormal;

void main( void ) { 

  // gl_Position = uMVP * vec4(aPosition + aInstancePosition + vec3(aInstancePosI, 0, 0), 1.0);
  gl_Position = uMVP * vec4(aPosition + aInstancePosition, 1.0);

  vNormal = aNormal;
}
