/* bragg.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__2 = 2;
static integer c__9 = 9;
static integer c__1 = 1;
static integer c__0 = 0;

/* Subroutine */ int braggit_(x1, x2, c1, c2, a0, ah)
doublecomplex *x1, *x2, *c1, *c2, *a0, *ah;
{
    /* System generated locals */
    integer i__1;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer ipvt[2];
    static doublecomplex a[4]	/* was [2][2] */, b[2];
    static integer i;
    extern /* Subroutine */ int cgeco_();
    static doublecomplex z[2];
    extern /* Subroutine */ int cgesl_();
    static doublereal rcond;

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 6, 0, 0, 0 };



/* --- der braggfall */


    a[0].r = 1., a[0].i = 0.;
    a[2].r = 1., a[2].i = 0.;

    z__1.r = c1->r * x1->r - c1->i * x1->i, z__1.i = c1->r * x1->i + c1->i * 
	    x1->r;
    a[1].r = z__1.r, a[1].i = z__1.i;
    z__1.r = c2->r * x2->r - c2->i * x2->i, z__1.i = c2->r * x2->i + c2->i * 
	    x2->r;
    a[3].r = z__1.r, a[3].i = z__1.i;

    cgeco_(a, &c__2, &c__2, ipvt, &rcond, z);

    if (rcond + 1. == 1.) {
	s_wsle(&io___5);
	do_lio(&c__9, &c__1, "Singulaere Matrix ", 18L);
	e_wsle();
	return 0;
    }

    b[0].r = 1., b[0].i = 0.;

    for (i = 2; i <= 2; ++i) {
	i__1 = i - 1;
	b[i__1].r = 0., b[i__1].i = 0.;
    }

    cgesl_(a, &c__2, &c__2, ipvt, b, &c__0);

    z__2.r = c1->r * b[0].r - c1->i * b[0].i, z__2.i = c1->r * b[0].i + c1->i 
	    * b[0].r;
    z__3.r = c2->r * b[1].r - c2->i * b[1].i, z__3.i = c2->r * b[1].i + c2->i 
	    * b[1].r;
    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
    a0->r = z__1.r, a0->i = z__1.i;

    z__2.r = x1->r * b[0].r - x1->i * b[0].i, z__2.i = x1->r * b[0].i + x1->i 
	    * b[0].r;
    z__3.r = x2->r * b[1].r - x2->i * b[1].i, z__3.i = x2->r * b[1].i + x2->i 
	    * b[1].r;
    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
    ah->r = z__1.r, ah->i = z__1.i;

    return 0;
} /* braggit_ */


/* Subroutine */ int laueit_(x1, x2, c1, c2, a0, ah)
doublecomplex *x1, *x2, *c1, *c2, *a0, *ah;
{
    /* System generated locals */
    integer i__1;
    doublecomplex z__1, z__2, z__3, z__4, z__5;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer ipvt[2];
    static doublecomplex a[4]	/* was [2][2] */, b[2];
    static integer i;
    extern /* Subroutine */ int cgeco_();
    static doublecomplex z[2];
    extern /* Subroutine */ int cgesl_();
    static doublereal rcond;

    /* Fortran I/O blocks */
    static cilist io___12 = { 0, 6, 0, 0, 0 };



/* --- der lauefall */


    a[0].r = 1., a[0].i = 0.;
    a[2].r = 1., a[2].i = 0.;

    a[1].r = x1->r, a[1].i = x1->i;
    a[3].r = x2->r, a[3].i = x2->i;

    cgeco_(a, &c__2, &c__2, ipvt, &rcond, z);

    if (rcond + 1. == 1.) {
	s_wsle(&io___12);
	do_lio(&c__9, &c__1, "Singulaere Matrix ", 18L);
	e_wsle();
	return 0;
    }

    b[0].r = 1., b[0].i = 0.;

    for (i = 2; i <= 2; ++i) {
	i__1 = i - 1;
	b[i__1].r = 0., b[i__1].i = 0.;
    }

    cgesl_(a, &c__2, &c__2, ipvt, b, &c__0);

    z__2.r = c1->r * b[0].r - c1->i * b[0].i, z__2.i = c1->r * b[0].i + c1->i 
	    * b[0].r;
    z__3.r = c2->r * b[1].r - c2->i * b[1].i, z__3.i = c2->r * b[1].i + c2->i 
	    * b[1].r;
    z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
    a0->r = z__1.r, a0->i = z__1.i;

    z__3.r = c1->r * x1->r - c1->i * x1->i, z__3.i = c1->r * x1->i + c1->i * 
	    x1->r;
    z__2.r = z__3.r * b[0].r - z__3.i * b[0].i, z__2.i = z__3.r * b[0].i + 
	    z__3.i * b[0].r;
    z__5.r = c2->r * x2->r - c2->i * x2->i, z__5.i = c2->r * x2->i + c2->i * 
	    x2->r;
    z__4.r = z__5.r * b[1].r - z__5.i * b[1].i, z__4.i = z__5.r * b[1].i + 
	    z__5.i * b[1].r;
    z__1.r = z__2.r + z__4.r, z__1.i = z__2.i + z__4.i;
    ah->r = z__1.r, ah->i = z__1.i;

    return 0;
} /* laueit_ */

