#include <MiniFB.h>
#include <stdint.h>
#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "stb_image_write.h"

int run_render_loop() {}

int save_canvas(const char *filename, int width, int height,
                const unsigned char *pixels) {

  return stbi_write_png(filename, width, height, 3, pixels, 3 * width);
}
