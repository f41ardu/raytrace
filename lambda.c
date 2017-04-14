/* lambda.f -- translated by f2c (version of 27 June 1992  14:50:07).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

#include "f2c.h"

doublereal lmbda_(h1, k1, l1, h2, k2, l2)
doublereal *h1, *k1, *l1, *h2, *k2, *l2;
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal habs, labs, d, hlabs;


/* --- berechnung der wellenlaenge im dreistrahlfall nach */
/*     Mack / Gitterkonstante aus Teworte dr. arbeit seite 73 */

/*        d = 1.d0/0.18423d0 */
    d = 5.43102018;
    habs = sqrt(*h1 * *h1 + *k1 * *k1 + *l1 * *l1);
    labs = sqrt(*h2 * *h2 + *k2 * *k2 + *l2 * *l2);
/* Computing 2nd power */
    d__1 = *h1 - *h2;
/* Computing 2nd power */
    d__2 = *k1 - *k2;
/* Computing 2nd power */
    d__3 = *l1 - *l2;
    hlabs = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
/* Computing 2nd power */
    d__1 = *h1 * *k2 - *h2 * *k1;
/* Computing 2nd power */
    d__2 = *h1 * *l2 - *h2 * *l1;
/* Computing 2nd power */
    d__3 = *k1 * *l2 - *k2 * *l1;
    ret_val = d * 2. * sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3) / (habs *
	     labs * hlabs);
    return ret_val;
} /* lmbda_ */

