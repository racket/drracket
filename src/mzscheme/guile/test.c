#include <stdio.h>
#include <math.h>

#include "gh.h"

SCM c_factorial(SCM s_n);
SCM c_sin(SCM s_x);
SCM c_vector_test(SCM s_length);

void real_main(int argc, char *argv[])
{
  int done;
  char input_str[1000];
     
  gh_eval_str("(display \"hello guile: try square, factorial, of c_{factorial,sin,vector_test}\n\")");
     
  gh_eval_str("(define (square x) (* x x))");
  gh_eval_str("(define (factorial n) (if (= n 1) 1 (* n (factorial (- n 1)))))");
     
  gh_eval_str("(square 9)");
  gh_eval_str("(factorial 100)");
     
  /* now define some new primitives in C */
  gh_new_procedure("c_factorial", c_factorial, 1, 0, 0);
  gh_new_procedure("c_sin", c_sin, 1, 0, 0);
  gh_new_procedure("c_vector_test", c_vector_test, 1, 0, 0);
     
  /* now sit in a Scheme eval loop: I input the expressions, have guile
   * evaluate them, and then get another expression.
   */
  done = 0;
  while (!done) {
    if (gets(input_str) == NULL || strcmp(input_str, "(quit)") == 0) {
      done = 1;
    } else {
      gh_eval_str(input_str);
    }
  }
}

int main(int argc, char *argv[])
{
  gh_enter(argc, argv, real_main);

  return 0;
}

SCM c_factorial(SCM s_n)
{
  int i, n;
  unsigned long result = 1;
     
  n = gh_scm2ulong(s_n);
     
  for (i = 1; i <= n; ++i) {
    result = result*i;
  }
  return gh_ulong2scm(result);
}

/* a sin routine in C, callable from Scheme.  it is named c_sin()
 * to distinguish it from the default Scheme sin function
 */
SCM c_sin(SCM s_x)
{
  double x = gh_scm2double(s_x);
     
  return gh_double2scm(sin(x));
}

/* play around with vectors in guile: this routine creates a
 * vector of the given length, initializes it all to zero except
 * element 2 which is set to 1.9.
 */
SCM c_vector_test(SCM s_length)
{
  SCM xvec;
  unsigned long c_length;
     
  c_length = gh_scm2ulong(s_length);
  printf("requested length for vector: %ld\n", c_length);
     
  /* create a vector filled witth 0.0 entries */
  xvec = gh_vector(c_length, gh_double2scm(0.0));
  /* set the second element in it to some floating point value */
  gh_vset(xvec, 2, gh_double2scm(1.9));
     
  return xvec;
}
