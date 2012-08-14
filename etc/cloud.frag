#version 110

void main() {
  //gl_FragColor = vec4(0,255,0,1);
  //gl_FragColor = vec4(0, pow(1.0 / gl_FragCoord.z, 16.0), 0, 1);
  //gl_FragColor = vec4(0, pow(gl_FragCoord.z, 2.0), 0, 1);
  gl_FragColor = vec4(0, gl_FragCoord.w + 0.5, 0, 1);
  //gl_FragColor = vec4(0,1,0,1);
}
