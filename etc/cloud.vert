#version 110
attribute vec3 vertexCoord;
uniform mat4 cam;

void main() {
  const float focal = 1.0;
  //vec4 tmp = vec4(vertexCoord.xy, (vertexCoord.z-5.0)*10.0+10.0, 1);
  //vec4 v = cam * tmp;
  vec4 v = cam * vec4(vertexCoord,1);
  //gl_PointSize = 1.0 / (5.0 - v.z);
  //gl_PointSize = 10.0;
  //float depth = -v.z * 10000.0;
  //gl_PointSize = 100.0 / depth;
  //if(v.w > 3.0) gl_PointSize = 10.0;
  //else gl_PointSize = 1.0;
  gl_PointSize = 1.0 / max(0.1, v.w - 1.0);
  //gl_PointSize = max(1.0, -1.0 * v.z);
  //gl_PointSize = max(1.0, min(10.0, 1.0/(2.0 - v.z)));
  gl_Position = vec4(focal * v.x,
                     focal * v.y,
                     v.z,
                     v.w);
}
