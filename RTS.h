/* Unlambda-via-Scheme runtime */
/* If a compiled program contained the `d` builtin, then
   USED_D must be defined before including this. It should be defined
   as a value N such that the case labels N and N+1 are unused by the program. */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> // intptr_t

#ifndef HEAP_SIZE
#define HEAP_SIZE 1000000
#endif

typedef intptr_t obj;
obj global[NB_GLOBALS];
obj stack[MAX_STACK];
obj heap[HEAP_SIZE];

#define CLOS2OBJ(c) ((obj)c)
#define PROM2OBJ(p) ((obj)p + 1)
#define OBJ2PTR(o)  ((obj *)(o & ~1))

#define GLOBAL(i) global[i]
#define LOCAL(i)  stack[i]
#define CLOSURE_REF(self, i) OBJ2PTR(self)[i]

#define TOS()   sp[-1]
#define PUSH(x) *sp++ = x
#define POP()   *--sp

#define DOT_C(c) printf("%c", c); PUSH(0);
#define PTR_EQ() { obj y = POP(); TOS() = TOS() == y; }
#define HALT() break

#define BEGIN_CLOSURE(label, nwords) if (hp - (nwords + 1) < heap) hp = gc(sp);
#define INICLO(i) *--hp = POP()
#define END_CLOSURE(label, nwords) *--hp = label; PUSH(CLOS2OBJ(hp));
#define END_PROMISE(label, nwords) *--hp = label; PUSH(PROM2OBJ(hp));

#define BEGIN_JUMP(nwords) sp = stack;

#ifdef USED_D
#define END_JUMP(nwords) {                       \
    if (LOCAL(0) & 1) pc = USED_D;               \
    else              pc = OBJ2PTR(LOCAL(0))[0]; \
    goto jump;                                   \
  }
// When we jump to a promise, we do need to actually enter it!
// if we simply END_JUMP, we will see (again) that it is a promise and 
// we will jump to USED_D again, quickly causing memory exhaustion by creating
// infinite apply continuations.
#define FORCE_END_JUMP(nwords) pc = OBJ2PTR(LOCAL(0))[0]; goto jump;
#else
#define END_JUMP(nwords) pc = OBJ2PTR(LOCAL(0))[0]; goto jump;
#endif

obj *gc(obj *sp) { fprintf(stderr, "Out of memory (no garbage collector)"); exit(1); }

obj execute() {
  int pc = 0;
  obj *sp = stack;
  obj *hp = &heap[HEAP_SIZE];
  
  jump: switch (pc) {

#ifdef USED_D
case USED_D: /* force-apply */
  PUSH(LOCAL(1)); PUSH(LOCAL(2));
    BEGIN_CLOSURE(USED_D+1,2); INICLO(2); INICLO(1); END_CLOSURE(USED_D+1,2);
  BEGIN_JUMP(2); PUSH(LOCAL(0)); PUSH(LOCAL(3)); FORCE_END_JUMP(2);

case (USED_D + 1): /* apply */
  PUSH(LOCAL(0)); TOS() = CLOSURE_REF(TOS(),1);
  PUSH(LOCAL(0)); TOS() = CLOSURE_REF(TOS(),2);
  BEGIN_JUMP(3); PUSH(LOCAL(1)); PUSH(LOCAL(2)); PUSH(LOCAL(3)); END_JUMP(3);
#endif