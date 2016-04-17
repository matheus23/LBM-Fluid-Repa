#version 130

in vec2 position;

out vec4 fragmentColor;

void main() {
  vec2 normPos = (position + vec2(1)) * 0.5;
  fragmentColor = vec4(normPos, 1, 1);
}
