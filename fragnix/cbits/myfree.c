#include "myfree.h"
#include "regex.h"
#include "stdlib.h"

/* 
void free(void *ptr);
void regfree(regex_t *preg);
*/

void myregfree(void *preg) {
  regfree(preg);
  free(preg);
}
