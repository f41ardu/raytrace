/* dreh.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int dreh_(m, x_input__, x_out__)
doublereal *m, *x_input__, *x_out__;
{
    static doublereal x_in__[3];
    static integer i;


/* --- Koordinatendrehung */

    /* Parameter adjustments */
    --x_out__;
    --x_input__;
    m -= 4;

    /* Function Body */
    for (i = 1; i <= 3; ++i) {
	x_in__[i - 1] = x_input__[i];
    }
    x_out__[1] = m[4] * x_in__[0] + m[7] * x_in__[1] + m[10] * x_in__[2];
    x_out__[2] = m[5] * x_in__[0] + m[8] * x_in__[1] + m[11] * x_in__[2];
    x_out__[3] = m[6] * x_in__[0] + m[9] * x_in__[1] + m[12] * x_in__[2];
    return 0;
} /* dreh_ */


/* Subroutine */ int bdreh_(m, xd, x_in__, x_out__)
doublereal *m, *xd, *x_in__, *x_out__;
{
    extern /* Subroutine */ int dreh_();
    static integer iloop;
    static doublereal gx[3];


/* --- drehung um einen beliebigen Punkt */

    /* Parameter adjustments */
    --x_out__;
    --x_in__;
    --xd;
    m -= 4;

    /* Function Body */
    for (iloop = 1; iloop <= 3; ++iloop) {
	gx[iloop - 1] = x_in__[iloop] - xd[iloop];
    }

/* --- Drehen des Richtungsvektors */

    dreh_(&m[4], gx, &x_out__[1]);

/* --- Ruecktranslation */

    for (iloop = 1; iloop <= 3; ++iloop) {
	x_out__[iloop] += xd[iloop];
    }

/* --- das war's */

    return 0;
} /* bdreh_ */


/* Subroutine */ int trans_(xd, x, x1)
doublereal *xd, *x, *x1;
{
    static integer iloop;


/* ---  translation eines punktes */


    /* Parameter adjustments */
    --x1;
    --x;
    --xd;

    /* Function Body */
    for (iloop = 1; iloop <= 3; ++iloop) {
	x1[iloop] = x[iloop] + xd[iloop];
    }

/* --- das war's */

    return 0;
} /* trans_ */


/* Subroutine */ int matrix_(alpha, beta, gamma, theta, m)
doublereal *alpha, *beta, *gamma, *theta, *m;
{
    /* Builtin functions */
    double cos(), sin();


/* --- erzeuge eine drehmatrix */


/* --- drehmatrix */

    /* Parameter adjustments */
    m -= 4;

    /* Function Body */
    m[4] = cos(*theta) + *alpha * *alpha * (1. - cos(*theta));
    m[7] = *gamma * sin(*theta) + *alpha * *beta * (1. - cos(*theta));
    m[10] = -(*beta) * sin(*theta) + *alpha * *gamma * (1. - cos(*theta));

    m[5] = -(*gamma) * sin(*theta) + *alpha * *beta * (1. - cos(*theta));
    m[8] = cos(*theta) + *beta * *beta * (1. - cos(*theta));
    m[11] = *alpha * sin(*theta) + *beta * *gamma * (1. - cos(*theta));

    m[6] = *beta * sin(*theta) + *gamma * *alpha * (1. - cos(*theta));
    m[9] = -(*alpha) * sin(*theta) + *gamma * *beta * (1. - cos(*theta));
    m[12] = cos(*theta) + *gamma * *gamma * (1. - cos(*theta));

/* --- und Tschuess */

    return 0;
} /* matrix_ */

