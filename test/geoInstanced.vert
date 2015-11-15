#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
in vec3 aInstanceOffset;
in int aInstanceI;

out vec3 vNormal;

void main( void ) { 

  gl_Position = uMVP * vec4(aPosition + aInstanceOffset + vec3(aInstanceI, 0, 0), 1.0);

  vNormal = aNormal;
}
