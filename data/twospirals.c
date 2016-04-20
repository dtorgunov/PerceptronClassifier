#include <stdio.h>
#include <math.h>

int main() {
  int i;
  double x, y, angle, radius;

  for (i=0; i<=96; i++) {
    angle = i * M_PI / 16.0;
    radius = 6.5 * (104-i)/104.0;
    x = radius * sin(angle);
    y = radius * cos(angle);
    printf("%8.5f,%8.5f,%3.1f\n", x, y, 1.0);
    printf("%8.5f,%8.5f,%3.1f\n", -x, -y, 0.0);
  }
}
