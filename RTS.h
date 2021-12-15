/* Unlambda-via-Scheme runtime */
/* If a compiled program contained the `d` builtin, then
   USED_D must be defined before including this. It should be defined
   as a value N such that the case labels N and N+1 are unused by the program. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h> // memcpy
#include <stdint.h> // intptr_t

#ifndef HEAP_SIZE
#define HEAP_SIZE 1000000000
#endif

typedef intptr_t obj;
obj global[NB_GLOBALS];
obj stack[MAX_STACK];
obj *heap;

#define CLOS2OBJ(c) ((obj)c)
#define PROM2OBJ(p) ((obj)p + 1)
#define OBJ2PTR(o)  ((obj *)(o & ~1))

#define FUNCTION(lbl) case lbl:

#define GLOBAL(i) global[i]
#define LOCAL(i)  stack[i]
#define CLOSURE_REF(i) TOS() = OBJ2PTR(TOS())[i]

#define TOS()   sp[-1]
#define PUSH(x) *sp++ = x
#define POP()   *--sp

#define DOT_C(c) printf("%c", c); PUSH(0);
#define PTR_EQ() { obj y = POP(); TOS() = TOS() == y; }
#define HALT() break

#define MAKE_CLOSURE(lbl,size) {             \
  if (hp - (size + 1) < heap) hp = gc(sp);   \
  hp -= size + 1;                            \
  *hp = lbl;                                 \
  memcpy(hp+1, sp-size, size * sizeof(*hp)); \
  sp -= size;                                \
  PUSH(CLOS2OBJ(hp));                        \
  }
#define MAKE_PROMISE(lbl,size) \
  MAKE_CLOSURE(lbl,size);      \
  TOS() = PROM2OBJ(OBJ2PTR(TOS()));

#define JUMP(nargs) {                            \
    memcpy(stack,sp-nargs,nargs*sizeof(*sp));    \
    sp = stack+nargs;                            \
    if (LOCAL(0) & 1) pc = USED_D;               \
    else              pc = OBJ2PTR(LOCAL(0))[0]; \
    goto jump;                                   \
  }
// When we jump to a promise, we do need to actually enter it!
// if we simply END_JUMP, we will see (again) that it is a promise and 
// we will jump to USED_D again, quickly causing memory exhaustion by creating
// infinite apply continuations.
#define FORCE_JUMP(nargs) {             \
  memcpy(stack,sp-nargs,nargs*sizeof(*sp)); \
  sp = stack+nargs;                         \
  pc = OBJ2PTR(LOCAL(0))[0];                \
  goto jump;                                \
  }

obj *gc(obj *sp) { fprintf(stderr, "Out of memory (no garbage collector)"); exit(1); }

obj execute() {
  int pc = 0;
  obj *sp = stack;
  heap = malloc(HEAP_SIZE*sizeof(obj));
  obj *hp = &heap[HEAP_SIZE];
  
  jump: switch (pc) {

#ifdef USED_D
case USED_D: /* force-apply */
  PUSH(LOCAL(0));
  PUSH(LOCAL(1)); PUSH(LOCAL(2)); MAKE_CLOSURE(USED_D+1,2);
  FORCE_JUMP(2);

case (USED_D + 1): /* apply */
  PUSH(LOCAL(1));
  PUSH(LOCAL(0)); CLOSURE_REF(1);
  PUSH(LOCAL(0)); CLOSURE_REF(2);
  JUMP(3);
#endif