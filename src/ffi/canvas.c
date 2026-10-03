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
  size_t frame_size;
  int height, width;
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
  uint16_t bpp = 0;
  size_t file_size = 0;

  char *temp_buf = NULL;
  anim.data = NULL;

  // generates prototype for all other files to follow.

  if (tinydir_open(&dir, name) == -1)
    goto error;

  if (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
      goto close;

    while (file.is_dir) {
      tinydir_next(&dir);
      if (tinydir_readfile(&dir, &file) == -1)
        goto close;
    }

    if (!file.is_dir) {
      FILE *loaded_file = fopen(file.path, "rb");
      if (loaded_file == NULL) {
        goto close;
      }

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
      memcpy(&bpp, temp_buf + 28, sizeof(uint16_t));

      file_size = file._s.st_size;
      anim.frame_size = bpp * anim.width * anim.height;

      printf("Animation %s: \nheight: %d, width: %d, bpp: %hu\n", name,
             anim.height, anim.width, bpp);
      file_count++;
    }
    tinydir_next(&dir);
  }

  // gets count of files, by finishing the iteration
  while (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
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

    tinydir_next(&dir);
  }
  tinydir_close(&dir);

  // second pass loads opens each file and saves it to the buffer
  // the first file becomes prototype for the rest of the files, if the bpp
  // width and height dont match you get an error
  if (tinydir_open(&dir, name) == -1){
     free((void *)temp_buf);
     goto error;
  }

  size_t bufsize = (anim.height * anim.width * (bpp / 8));
  anim.data = (char *)malloc(file_count * bufsize);
  if (anim.data == NULL)
    goto cleanup1;

  file_count = 0;

  while (dir.has_next) {
    if (tinydir_readfile(&dir, &file) == -1)
      goto cleanup2;

    if (!file.is_dir) {
      FILE *loaded_file = fopen(file.path, "rb");
      if (loaded_file == NULL) {
        fclose(loaded_file);
        goto cleanup2;
      }

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

      if (bpp != curr_bpp) {
        fprintf(stderr, "bpp in file name %s deviates\n", file.name);
        goto cleanup2;
      }

      uint32_t offset_from_file = 0;
      memcpy(&offset_from_file, temp_buf + 10, sizeof(uint32_t));
      memcpy(anim.data + file_count * bufsize, temp_buf + offset_from_file,
             bufsize);
      file_count++;
    }
    tinydir_next(&dir);
  }

  // success case
  tinydir_close(&dir);
  free((void *)temp_buf);
  return anim;

  // linux kernel style error handling here
cleanup2:
  free((void *)anim.data);
cleanup1:
  free((void *)temp_buf);
close:
  tinydir_close(&dir);
error:
  return (animation_t){NULL, 0, 0, 0};
}

void dealloc_anim(animation_t *anim) {
  free((void *)anim->data);
  anim->data = NULL;
}
