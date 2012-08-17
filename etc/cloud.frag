#version 110
uniform sampler1D heat;
void main() {
  //gl_FragColor = vec4(0, gl_FragCoord.w + 0.5, 0, 1);
  gl_FragColor = texture1D(heat, 1.0 - gl_FragCoord.w);
}
