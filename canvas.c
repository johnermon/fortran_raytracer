#include "MiniFB_types.h"
#include <MiniFB.h>
#include <stdint.h>
#include <stdio.h>
#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "stb_image_write.h"

struct mfb_window *open_window(const char *name, int width, int height) {
  struct mfb_window *window = mfb_open(name, width, height);
  return window;
}

void close_window(struct mfb_window *window) { mfb_close(window); }

int save_canvas(const char *filename, int width, int height,
                const unsigned char *pixels) {

  return stbi_write_png(filename, width, height, 3, pixels, 3 * width);
}
