#include "Fred.h"
#include "stdio.h"

int main() {
  printf("Calling c_new_fred\n");
  Fred* fred = c_new_fred();
  printf("Calling c_function(fred)\n");
  c_function(fred);
}
