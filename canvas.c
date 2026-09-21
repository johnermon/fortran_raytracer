#include "MiniFB_types.h"
#include <MiniFB.h>
#include <stdint.h>
#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "stb_image_write.h"

// here to coerce the signed int in input into unsigned int
struct mfb_window *mfb_open_singed_int(const char *name, int width,
                                       int height) {
  struct mfb_window *window = mfb_open(name, width, height);
  return window;
}

int save_canvas(const char *filename, int width, int height,
                const unsigned char *pixels) {

  return stbi_write_png(filename, width, height, 4, pixels, 4 * width);
}
