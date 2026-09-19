#include <stdint.h>

#define STB_IMAGE_WRITE_IMPLEMENTATION
#include "stb_image_write.h"

int save_canvas(const char *filename, int width, int height,
                const u_int8_t *pixels) {

  return stbi_write_png(filename, width, height, 3, pixels, 3 * width);
}
