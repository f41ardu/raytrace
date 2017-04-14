/* gtheta.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal gtheta_(h, k, l, lambda, imat)
doublereal *h, *k, *l, *lambda;
integer *imat;
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double sqrt(), asin();

    /* Local variables */
    static doublereal a, hkl;


/* --- bragg winkel */

    if (*imat == 1) {
	a = 5.430102018;
    }
/* Gi_kn SI */
    if (*imat == 2) {
	a = 3.567;
    }
    if (*imat == 3) {
	a = 5.658;
    }
    hkl = *h * *h + *k * *k + *l * *l;
    hkl = sqrt(hkl);
    ret_val = *lambda * hkl / (a * 2.);
    ret_val = asin(ret_val);
    return ret_val;
} /* gtheta_ */

