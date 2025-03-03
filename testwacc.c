#include <stdlib.h>
#include <stdio.h>

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
  sscanf(filename, "%[^.]", basename);
  
  // Run scala compiler - fixed the em dash and updated the command structure
  sprintf(command, "scala . -- %s", filename);  // Or try: sprintf(command, "scala shebang %s", filename);
  status = system(command);
  if (status != 0) {
    printf("Scala compilation failed\n");
    return status;
  }
  
  // Compile assembly to binary
  sprintf(command, "gcc -o %s %s.s", basename, basename);
  status = system(command);
  if (status != 0) {
    printf("GCC compilation failed\n");
    return status;
  }
  
  // Run the binary
  sprintf(command, "./%s", basename);
  status = system(command);
  
  return status;
}