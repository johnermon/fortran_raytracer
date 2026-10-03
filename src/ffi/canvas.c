#include "MiniFB_types.h"
#include <MiniFB.h>
#include <stddef.h>
#include <stdint.h>
#include <tinydir.h>

#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "stb_image_write.h"

// here to coerce the signed int in input into unsigned int
struct mfb_window *mfb_open_signed_int(const char *name, int width,
                                       int height) {
  struct mfb_window *window = mfb_open(name, width, height);
  return window;
}

int write_png(const char *filename, int width, int height,
              const unsigned char *pixels) {

  return stbi_write_png(filename, width, height, 4, pixels, 4 * width);
}

typedef struct animation {
  char *data;
  size_t frame_size;
  int height, width;
} animation_t;

// safety note, unsafe if files arent all the exact same size, and are bmps
// whatever this is c we play with fire amirite gamers
animation_t load_anim(const char *name) {
  animation_t anim;
  tinydir_dir dir;
  tinydir_file file;

  int file_count = 0;
  size_t alloc_size = 0;

  if (tinydir_open_sorted(&dir, name) == -1)
    goto error;

  if (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
      goto close;

    if (!file.is_dir) {
      anim.frame_size = file._s.st_size;
      file_count++;
      alloc_size += file._s.st_size;
    }

    tinydir_next(&dir);
  }

  while (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
      goto close;

    if (!file.is_dir) {
      file_count++;

      if (anim.frame_size != file._s.st_size) {
        fprintf(stderr,
                "frame number %d , file name %s, has a different size from "
                "previous frames",
                file_count, file.name);
        goto close;
      }

      alloc_size += file._s.st_size;
    }
    tinydir_next(&dir);
  }
  tinydir_close(&dir);

  if (tinydir_open_sorted(&dir, name) == -1)
    goto error;

  anim.data = (char *)malloc(alloc_size);
  size_t curr_index = 0;

  while (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
      goto cleanup;

    if (!file.is_dir) {
      FILE *loaded_file = fopen(file.path, "rb");
      size_t bytes_read =
          fread(anim.data + curr_index, 1, anim.frame_size, loaded_file);

      curr_index += anim.frame_size;
      fclose(loaded_file);
    }

    tinydir_next(&dir);
  }

  return anim;

  // linux kernel style error handling here
cleanup:
  free((void *)anim.data);
close:
  tinydir_close(&dir);
error:
  return (animation_t){NULL, 0, 0, 0};
}
