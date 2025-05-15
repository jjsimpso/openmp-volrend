#include <stdint.h>
#include <stdbool.h>


uint8_t *read_ppm(char *path, int *w, int *h);
int write_ppm(char *path, int width, int height, uint8_t *data);
NDArray *ndarray_read_ppm(char *path);
int ndarray_write_ppm(NDArray *nda, char *path);
int ndarray_iter_write_ppm(NDArrayIter *it, char *path, int width, int height);
