#ifndef FRED_H
#define FRED_H

#ifdef __cplusplus
class Fred {
 public:
  Fred();
  void wilma(int);
 private:
  int a_;
};
#else
typedef struct Fred {
  void* fred; // dummy
} Fred;
#endif

#ifdef __cplusplus
extern "C" {
#endif

  extern void c_function(Fred*);
  extern Fred* cplusplus_callback_function(Fred*);
  extern Fred* c_new_fred();
  extern Fred* new_fred();
  extern void c_wilma_with_new_fred();

/* #if defined(__STDC__) || defined(__cplusplus) */
/*   extern void c_function(Fred*); */
/*   extern Fred* cplusplus_callback_function(Fred*); */
/*   extern Fred* c_new_fred(); */
/*   extern Fred* new_fred(); */
/*   extern void c_wilma_with_new_fred(); */
/* #else */
/*   extern void c_function(); */
/*   extern Fred* cplusplus_callback_function(); */
/*   extern Fred* c_new_fred(); */
/*   extern Fred* new_fred(); */
/*   extern void c_wilma_with_new_fred(); */
/* #endif */

#ifdef __cplusplus
}
#endif

#endif
