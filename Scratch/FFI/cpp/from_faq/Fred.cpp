#include "Fred.h"
#include <iostream>

using namespace std;

Fred::Fred() : a_(0) { }

void Fred::wilma(int a) {
  cout << "wilma: " << a << endl;
}

Fred* cplusplus_callback_function(Fred* fred) {
  fred->wilma(123);
  return fred;
}

Fred* new_fred() {
  return (Fred*)new Fred;
}
