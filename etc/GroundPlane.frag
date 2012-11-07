#version 110
uniform vec3 wireColor;

void main() {
  gl_FragColor = vec4(wireColor,1.0);
}
