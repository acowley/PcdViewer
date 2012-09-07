#version 110
uniform sampler1D heat;
void main() {
  //gl_FragColor = texture1D(heat, gl_FragCoord.z * 2.0); // works okay
  const float far = 15.0;
  float depth = (1.0 / gl_FragCoord.w) / far;
  gl_FragColor = texture1D(heat, depth);
}
