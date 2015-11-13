#version 330 core

uniform mat4 uMVP;

in vec3 aPosition;
in vec3 aNormal;
in vec3 aInstanceOffset;

out vec3 vNormal;

void main( void ) { 

  gl_Position = uMVP * vec4(aPosition + aInstanceOffset, 1.0);

  vNormal = aNormal;
}
