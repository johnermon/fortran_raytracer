#include "MiniFB_types.h"
#include <MiniFB.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
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
  int height, width, frame_count;
  uint16_t bpp;
} animation_t;

// i hate this function its 200 lines long but because each pipeline step works
// on many variables in the program it is very hard to split it into many
// functions without passing 3 references into a bunch of helper functions.oh
// well. this is c we play with fire amirite gamers
animation_t load_anim(const char *name) {
  animation_t anim;
  tinydir_dir dir;
  tinydir_file file;

  int file_count = 0;
  size_t file_size = 0;
  size_t frame_size = 0;

  char *temp_buf = NULL;
  anim.data = NULL;

  // generates prototype for all other files to follow.

  size_t iter = 0;

  if (tinydir_open_sorted(&dir, name) == -1)
    goto error;

  if (tinydir_readfile_n(&dir, &file, iter) == -1)
    goto close;

  while (file.is_dir) {
    iter++;
    if (tinydir_readfile_n(&dir, &file, iter) == -1)
      goto close;
  }

  FILE *loaded_file = fopen(file.path, "rb");
  if (loaded_file == NULL)
    goto close;

  temp_buf = (char *)malloc(file._s.st_size);

  if (temp_buf == NULL) {
    fclose(loaded_file);
    goto close;
  }

  if (fread(temp_buf, file._s.st_size, 1, loaded_file) != 1) {
    fclose(loaded_file);
    goto cleanup1;
  }

  fclose(loaded_file);

  memcpy(&anim.width, temp_buf + 18, sizeof(int));
  memcpy(&anim.height, temp_buf + 22, sizeof(int));
  memcpy(&anim.bpp, temp_buf + 28, sizeof(uint16_t));

  file_size = file._s.st_size;
  frame_size = (anim.bpp / 8) * anim.width * anim.height;
  file_count++;
  iter++;

  // gets count of files, by finishing the iteration
  for (size_t i = iter; i < dir.n_files; i++) {
    if (tinydir_readfile_n(&dir, &file, i) == -1)
      goto cleanup1;

    if (!file.is_dir) {
      file_count++;

      if (file_size != file._s.st_size) {
        fprintf(stderr,
                "frame number %d , file name %s, has a different size from "
                "previous frames",
                file_count, file.name);
        goto cleanup1;
      }
    }
  }

  // second pass loads opens each file and saves it to the buffer
  // the first file becomes prototype for the rest of the files, if the bpp
  // width and height dont match you get an error
  anim.data = (char *)malloc(file_count * frame_size);
  if (anim.data == NULL)
    goto cleanup1;

  anim.frame_count = file_count;
  file_count = 0;

  for (size_t i = 0; i < dir.n_files; i++) {
    if (tinydir_readfile_n(&dir, &file, i) == -1)
      goto cleanup2;

    if (!file.is_dir) {
      FILE *loaded_file = fopen(file.path, "rb");
      if (loaded_file == NULL)
        goto cleanup2;

      if (fread(temp_buf, file._s.st_size, 1, loaded_file) != 1) {
        fclose(loaded_file);
        goto cleanup2;
      }

      fclose(loaded_file);

      int height = 0, width = 0;
      uint16_t curr_bpp;

      memcpy(&width, temp_buf + 18, sizeof(int));
      memcpy(&height, temp_buf + 22, sizeof(int));
      memcpy(&curr_bpp, temp_buf + 28, sizeof(uint16_t));

      if (anim.width != width || anim.height != height) {
        fprintf(stderr, "dimensions in file name %s deviates\n", file.name);
        goto cleanup2;
      }

      if (anim.bpp != curr_bpp) {
        fprintf(stderr, "bpp in file name %s deviates\n", file.name);
        goto cleanup2;
      }

      uint32_t offset_from_file = 0;
      memcpy(&offset_from_file, temp_buf + 10, sizeof(uint32_t));
      memcpy(anim.data + file_count * frame_size, temp_buf + offset_from_file,
             frame_size);

      file_count++;
    }
  }

  // success case
  tinydir_close(&dir);
  free((void *)temp_buf);
  return anim;

  // error case, linux kernel style error handling here
cleanup2:
  free((void *)anim.data);
cleanup1:
  free((void *)temp_buf);
close:
  tinydir_close(&dir);
error:
  return (animation_t){NULL, 0, 0, 0};
}
