#version 110

attribute vec2 vertexCoord;

uniform mat4 cam;
/* Identify which Euclidean plane is the ground plane */
/* X = 0 */
/* Y = 1 */
/* Z = 2 */
uniform int euclideanGround;

void main() {
  //const float focal = 0.5; // fisheye
  const float focal = 1.0;
  vec4 pt;

  if(euclideanGround == 0) {
    pt = vec4(0.0, vertexCoord, 1.0);
  } else if(euclideanGround == 1) {
    pt = vec4(vertexCoord.x, 0.0, vertexCoord.y, 1.0);
  } else {
    pt = vec4(vertexCoord, 0.0, 1.0);
  }
  vec4 v = cam * pt;
    
  gl_Position = vec4(focal * v.x,
                     focal * v.y,
                     v.z,
                     v.w);
}
