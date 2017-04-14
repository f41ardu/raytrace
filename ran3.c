/* ran3.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal ran3_(idum)
integer *idum;
{
    /* Initialized data */

    static integer iff = 0;

    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer i, k, inext, ma[55], ii, mj, mk, inextp;

/*         IMPLICIT REAL*4(M) */
/*         PARAMETER (MBIG=4000000.,MSEED=1618033.,MZ=0.,FAC=2.5E-7) */
    if (*idum < 0 || iff == 0) {
	iff = 1;
	mj = 161803398 - abs(*idum);
	mj %= 1000000000;
	ma[54] = mj;
	mk = 1;
	for (i = 1; i <= 54; ++i) {
	    ii = i * 21 % 55;
	    ma[ii - 1] = mk;
	    mk = mj - mk;
	    if (mk < 0) {
		mk += 1000000000;
	    }
	    mj = ma[ii - 1];
/* L11: */
	}
	for (k = 1; k <= 4; ++k) {
	    for (i = 1; i <= 55; ++i) {
		ma[i - 1] -= ma[(i + 30) % 55];
		if (ma[i - 1] < 0) {
		    ma[i - 1] += 1000000000;
		}
/* L12: */
	    }
/* L13: */
	}
	inext = 0;
	inextp = 31;
	*idum = 1;
    }
    ++inext;
    if (inext == 56) {
	inext = 1;
    }
    ++inextp;
    if (inextp == 56) {
	inextp = 1;
    }
    mj = ma[inext - 1] - ma[inextp - 1];
    if (mj < 0) {
	mj += 1000000000;
    }
    ma[inext - 1] = mj;
    ret_val = mj * 1e-9;
    return ret_val;
} /* ran3_ */

