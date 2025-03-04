#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <path/to/wacc/file>\n", argv[0]);
    return 1;
  }
  
  char command[1024];
  int status;
  
  // Extract basename without extension
  char *filename = argv[1];
  char basename[256];
  char *lastSlash = strrchr(filename, '/');
  if (lastSlash) {
    sscanf(lastSlash + 1, "%[^.]", basename);
  } else {
    sscanf(filename, "%[^.]", basename);
  }
  
  printf("Processing file: %s (basename: %s)\n", filename, basename);
  
  // Run scala compiler
  sprintf(command, "scala . -- %s", filename);
  printf("Running: %s\n", command);
  status = system(command);
  if (status != 0) {
    printf("Scala compilation failed with status %d\n", status);
    return status;
  }
  
  // Compile assembly to binary (from root directory)
  sprintf(command, "gcc -o ./%s ./%s.s", basename, basename);
  printf("Running: %s\n", command);
  status = system(command);
  if (status != 0) {
    printf("GCC compilation failed\n");
    return status;
  }
  
  // Run the binary (from root directory)
  sprintf(command, "./%s", basename);
  printf("Running: %s\n", command);
  status = system(command);
  
  return status;
}