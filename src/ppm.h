#include <stdint.h>
#include <stdbool.h>


uint8_t *read_ppm(char *path, int *w, int *h);
int write_ppm(char *path, int width, int height, uint8_t *data);
