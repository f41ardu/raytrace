/* gasdev.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;


doublereal gasdev_(idum)
integer *idum;
{
    /* Initialized data */

    static integer iset = 0;

    /* System generated locals */
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double log(), sqrt();

    /* Local variables */
    static doublereal gset, r, v1, v2, fac;
    extern doublereal ran3_();

    if (iset == 0) {
L1:
	v1 = ran3_(&c__1) * 2. - 1.;
	v2 = ran3_(&c__1) * 2. - 1.;
/* Computing 2nd power */
	d__1 = v1;
/* Computing 2nd power */
	d__2 = v2;
	r = d__1 * d__1 + d__2 * d__2;
	if (r >= 1.) {
	    goto L1;
	}
	fac = sqrt(log(r) * -2. / r);
	gset = v1 * fac;
	ret_val = v2 * fac;
	iset = 1;
    } else {
	ret_val = gset;
	iset = 0;
    }
    return ret_val;
} /* gasdev_ */

