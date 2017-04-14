/* strahl.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__0 = 0;

/* Subroutine */ int strahl_generieren__(strahl_p0__, strahl_a__, lambda)
doublereal *strahl_p0__, *strahl_a__, *lambda;
{
    static doublereal hdiv, vdiv, div_y__, div_z__;
    extern doublereal gasdev_(), ran3_();


/* --- erzeugt einen Strahl in der Quelle */
/*     mit DORIS Parametern G3 */
/* function's */

/* --- diverse divergenzparameter der maschine */

    /* Parameter adjustments */
    --strahl_a__;
    --strahl_p0__;

    /* Function Body */
    div_y__ = 1.067;
    div_z__ = .456;
    hdiv = .0042100000000000002;
    vdiv = 2.7300000000000002e-4;

    strahl_p0__[1] = 0.;
    strahl_p0__[2] = gasdev_(&c__0) * div_y__;
    strahl_p0__[3] = gasdev_(&c__0) * div_z__;

    strahl_a__[1] = 1.;
    strahl_a__[2] = gasdev_(&c__0) * hdiv;
    strahl_a__[3] = gasdev_(&c__0) * vdiv;
    *lambda = ran3_(&c__0);

    return 0;
} /* strahl_generieren__ */

