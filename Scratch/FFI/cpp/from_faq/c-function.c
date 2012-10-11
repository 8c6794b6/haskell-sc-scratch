#include "Fred.h"

void c_function(Fred* fred) {
  cplusplus_callback_function(fred);
}

Fred* c_new_fred(void) {
  return new_fred();
}

void c_wilma_with_new_fred() {
  Fred* fred = c_new_fred();
  c_function(fred);
}
