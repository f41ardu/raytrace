/* blc8.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublecomplex c_b24 = {1.,0.};
static doublereal c_b72 = 1.;

/*   23/11/92 211231441  MEMBER NAME  BLC8     (EISPACK.)    FVS */
/* Subroutine */ int caxpy_(n, ca, cx, incx, cy, incy)
integer *n;
doublecomplex *ca, *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double d_imag();

    /* Local variables */
    static integer i, ix, iy;


/*     CONSTANT TIMES A VECTOR PLUS A VECTOR. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if ((d__1 = ca->r, abs(d__1)) + (d__2 = d_imag(ca), abs(d__2)) == 0.) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS */
/*          NOT EQUAL TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = iy;
	i__3 = iy;
	i__4 = ix;
	z__2.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, z__2.i = ca->r * cx[
		i__4].i + ca->i * cx[i__4].r;
	z__1.r = cy[i__3].r + z__2.r, z__1.i = cy[i__3].i + z__2.i;
	cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	i__4 = i;
	z__2.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, z__2.i = ca->r * cx[
		i__4].i + ca->i * cx[i__4].r;
	z__1.r = cy[i__3].r + z__2.r, z__1.i = cy[i__3].i + z__2.i;
	cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
/* L30: */
    }
    return 0;
} /* caxpy_ */

/* Subroutine */ int ccopy_(n, cx, incx, cy, incy)
integer *n;
doublecomplex *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i, ix, iy;


/*     COPIES A VECTOR, X, TO A VECTOR, Y. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS */
/*          NOT EQUAL TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = iy;
	i__3 = ix;
	cy[i__2].r = cx[i__3].r, cy[i__2].i = cx[i__3].i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	cy[i__2].r = cx[i__3].r, cy[i__2].i = cx[i__3].i;
/* L30: */
    }
    return 0;
} /* ccopy_ */

/* Double Complex */ int cdotc_( ret_val, n, cx, incx, cy, incy)
doublecomplex * ret_val;
integer *n;
doublecomplex *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void d_cnjg();

    /* Local variables */
    static integer i;
    static doublecomplex ctemp;
    static integer ix, iy;


/*     FORMS THE DOT PRODUCT OF TWO VECTORS, CONJUGATING THE FIRST */
/*     VECTOR. */
/*     JACK DONGARRA, LINPACK,  3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    ctemp.r = 0., ctemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS */
/*          NOT EQUAL TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d_cnjg(&z__3, &cx[ix]);
	i__2 = iy;
	z__2.r = z__3.r * cy[i__2].r - z__3.i * cy[i__2].i, z__2.i = z__3.r * 
		cy[i__2].i + z__3.i * cy[i__2].r;
	z__1.r = ctemp.r + z__2.r, z__1.i = ctemp.i + z__2.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return ;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	d_cnjg(&z__3, &cx[i]);
	i__2 = i;
	z__2.r = z__3.r * cy[i__2].r - z__3.i * cy[i__2].i, z__2.i = z__3.r * 
		cy[i__2].i + z__3.i * cy[i__2].r;
	z__1.r = ctemp.r + z__2.r, z__1.i = ctemp.i + z__2.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return ;
} /* cdotc_ */

/* Double Complex */ int cdotu_( ret_val, n, cx, incx, cy, incy)
doublecomplex * ret_val;
integer *n;
doublecomplex *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublecomplex z__1, z__2;

    /* Local variables */
    static integer i;
    static doublecomplex ctemp;
    static integer ix, iy;


/*     FORMS THE DOT PRODUCT OF TWO VECTORS. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    ctemp.r = 0., ctemp.i = 0.;
     ret_val->r = 0.,  ret_val->i = 0.;
    if (*n <= 0) {
	return ;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS */
/*          NOT EQUAL TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	i__3 = iy;
	z__2.r = cx[i__2].r * cy[i__3].r - cx[i__2].i * cy[i__3].i, z__2.i = 
		cx[i__2].r * cy[i__3].i + cx[i__2].i * cy[i__3].r;
	z__1.r = ctemp.r + z__2.r, z__1.i = ctemp.i + z__2.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return ;

/*        CODE FOR BOTH INCREMENTS EQUAL TO 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	i__3 = i;
	z__2.r = cx[i__2].r * cy[i__3].r - cx[i__2].i * cy[i__3].i, z__2.i = 
		cx[i__2].r * cy[i__3].i + cx[i__2].i * cy[i__3].r;
	z__1.r = ctemp.r + z__2.r, z__1.i = ctemp.i + z__2.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
/* L30: */
    }
     ret_val->r = ctemp.r,  ret_val->i = ctemp.i;
    return ;
} /* cdotu_ */

doublereal cmach_(job)
integer *job;
{
    /* System generated locals */
    doublereal ret_val;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    void z_div();
    double sqrt();

    /* Local variables */
    static doublereal huge, tiny, s, eps;


/*     SMACH COMPUTES MACHINE PARAMETERS OF FLOATING POINT */
/*     ARITHMETIC FOR USE IN TESTING ONLY.  NOT REQUIRED BY */
/*     LINPACK PROPER. */

/*     IF TROUBLE WITH AUTOMATIC COMPUTATION OF THESE QUANTITIES, */
/*     THEY CAN BE SET BY DIRECT ASSIGNMENT STATEMENTS. */
/*     ASSUME THE COMPUTER HAS */

/*        B = BASE OF ARITHMETIC */
/*        T = NUMBER OF BASE  B  DIGITS */
/*        L = SMALLEST POSSIBLE EXPONENT */
/*        U = LARGEST POSSIBLE EXPONENT */

/*     THEN */

/*        EPS = B**(1-T) */
/*        TINY = 100.0*B**(-L+T) */
/*        HUGE = 0.01*B**(U-T) */

/*     DMACH SAME AS SMACH EXCEPT T, L, U APPLY TO */
/*     DOUBLE PRECISION. */

/*     CMACH SAME AS SMACH EXCEPT IF COMPLEX DIVISION */
/*     IS DONE BY */

/*        1/(X+I*Y) = (X-I*Y)/(X**2+Y**2) */

/*     THEN */

/*        TINY = SQRT(TINY) */
/*        HUGE = SQRT(HUGE) */


/*     JOB IS 1, 2 OR 3 FOR EPSILON, TINY AND HUGE, RESPECTIVELY. */



    eps = 1.;
L10:
    eps /= 2.;
    s = eps + 1.;
    if (s > 1.) {
	goto L10;
    }
    eps *= 2.;
    ret_val = eps;
    if (*job == 1) {
	return ret_val;
    }

    s = 1.;
L20:
    tiny = s;
    s /= 16.;
    if (s * 1. != 0.) {
	goto L20;
    }
    tiny = tiny / eps * 100.;
    z__2.r = tiny, z__2.i = 0.;
    z_div(&z__1, &c_b24, &z__2);
    s = z__1.r;
    if (s != 1. / tiny) {
	tiny = sqrt(tiny);
    }
    huge = 1. / tiny;
    if (*job == 1) {
	ret_val = eps;
    }
    if (*job == 2) {
	ret_val = tiny;
    }
    if (*job == 3) {
	ret_val = huge;
    }
    return ret_val;
} /* cmach_ */

/* Subroutine */ int crotg_(ca, cb, c, s)
doublecomplex *ca, *cb;
doublereal *c;
doublecomplex *s;
{
    /* System generated locals */
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    double z_abs(), sqrt();
    void d_cnjg();

    /* Local variables */
    static doublereal norm;
    static doublecomplex alpha;
    static doublereal scale;

    if (z_abs(ca) != 0.) {
	goto L10;
    }
    *c = 0.;
    s->r = 1., s->i = 0.;
    ca->r = cb->r, ca->i = cb->i;
    goto L20;
L10:
    scale = z_abs(ca) + z_abs(cb);
    z__1.r = ca->r / scale, z__1.i = ca->i / scale;
/* Computing 2nd power */
    d__1 = z_abs(&z__1);
    z__2.r = cb->r / scale, z__2.i = cb->i / scale;
/* Computing 2nd power */
    d__2 = z_abs(&z__2);
    norm = scale * sqrt(d__1 * d__1 + d__2 * d__2);
    d__1 = z_abs(ca);
    z__1.r = ca->r / d__1, z__1.i = ca->i / d__1;
    alpha.r = z__1.r, alpha.i = z__1.i;
    *c = z_abs(ca) / norm;
    d_cnjg(&z__3, cb);
    z__2.r = alpha.r * z__3.r - alpha.i * z__3.i, z__2.i = alpha.r * z__3.i + 
	    alpha.i * z__3.r;
    z__1.r = z__2.r / norm, z__1.i = z__2.i / norm;
    s->r = z__1.r, s->i = z__1.i;
    z__1.r = norm * alpha.r, z__1.i = norm * alpha.i;
    ca->r = z__1.r, ca->i = z__1.i;
L20:
    return 0;
} /* crotg_ */

/* Subroutine */ int cscal_(n, ca, cx, incx)
integer *n;
doublecomplex *ca, *cx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1;

    /* Local variables */
    static integer i, nincx;


/*     SCALES A VECTOR BY A CONSTANT. */
/*     JACK DONGARRA, LINPACK,  3/11/78. */


    /* Parameter adjustments */
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        CODE FOR INCREMENT NOT EQUAL TO 1 */

    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = i;
	i__4 = i;
	z__1.r = ca->r * cx[i__4].r - ca->i * cx[i__4].i, z__1.i = ca->r * cx[
		i__4].i + ca->i * cx[i__4].r;
	cx[i__3].r = z__1.r, cx[i__3].i = z__1.i;
/* L10: */
    }
    return 0;

/*        CODE FOR INCREMENT EQUAL TO 1 */

L20:
    i__2 = *n;
    for (i = 1; i <= i__2; ++i) {
	i__1 = i;
	i__3 = i;
	z__1.r = ca->r * cx[i__3].r - ca->i * cx[i__3].i, z__1.i = ca->r * cx[
		i__3].i + ca->i * cx[i__3].r;
	cx[i__1].r = z__1.r, cx[i__1].i = z__1.i;
/* L30: */
    }
    return 0;
} /* cscal_ */

/* Subroutine */ int csrot_(n, cx, incx, cy, incy, c, s)
integer *n;
doublecomplex *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
doublereal *c, *s;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2, z__3;

    /* Local variables */
    static integer i;
    static doublecomplex ctemp;
    static integer ix, iy;


/*     APPLIES A PLANE ROTATION, WHERE THE COS AND SIN (C AND S) ARE REAL 
*/
/*     AND THE VECTORS CX AND CY ARE COMPLEX. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL */
/*         TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	z__2.r = *c * cx[i__2].r, z__2.i = *c * cx[i__2].i;
	i__3 = iy;
	z__3.r = *s * cy[i__3].r, z__3.i = *s * cy[i__3].i;
	z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
	i__2 = iy;
	i__3 = iy;
	z__2.r = *c * cy[i__3].r, z__2.i = *c * cy[i__3].i;
	i__4 = ix;
	z__3.r = *s * cx[i__4].r, z__3.i = *s * cx[i__4].i;
	z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
	cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
	i__2 = ix;
	cx[i__2].r = ctemp.r, cx[i__2].i = ctemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       CODE FOR BOTH INCREMENTS EQUAL TO 1 */

L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	z__2.r = *c * cx[i__2].r, z__2.i = *c * cx[i__2].i;
	i__3 = i;
	z__3.r = *s * cy[i__3].r, z__3.i = *s * cy[i__3].i;
	z__1.r = z__2.r + z__3.r, z__1.i = z__2.i + z__3.i;
	ctemp.r = z__1.r, ctemp.i = z__1.i;
	i__2 = i;
	i__3 = i;
	z__2.r = *c * cy[i__3].r, z__2.i = *c * cy[i__3].i;
	i__4 = i;
	z__3.r = *s * cx[i__4].r, z__3.i = *s * cx[i__4].i;
	z__1.r = z__2.r - z__3.r, z__1.i = z__2.i - z__3.i;
	cy[i__2].r = z__1.r, cy[i__2].i = z__1.i;
	i__2 = i;
	cx[i__2].r = ctemp.r, cx[i__2].i = ctemp.i;
/* L30: */
    }
    return 0;
} /* csrot_ */

/* Subroutine */ int csscal_(n, sa, cx, incx)
integer *n;
doublereal *sa;
doublecomplex *cx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Builtin functions */
    double d_imag();

    /* Local variables */
    static integer i, nincx;


/*     SCALES A COMPLEX VECTOR BY A REAL CONSTANT. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        CODE FOR INCREMENT NOT EQUAL TO 1 */

    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = i;
	i__4 = i;
	d__1 = *sa * cx[i__4].r;
	d__2 = *sa * d_imag(&cx[i]);
	z__1.r = d__1, z__1.i = d__2;
	cx[i__3].r = z__1.r, cx[i__3].i = z__1.i;
/* L10: */
    }
    return 0;

/*        CODE FOR INCREMENT EQUAL TO 1 */

L20:
    i__2 = *n;
    for (i = 1; i <= i__2; ++i) {
	i__1 = i;
	i__3 = i;
	d__1 = *sa * cx[i__3].r;
	d__2 = *sa * d_imag(&cx[i]);
	z__1.r = d__1, z__1.i = d__2;
	cx[i__1].r = z__1.r, cx[i__1].i = z__1.i;
/* L30: */
    }
    return 0;
} /* csscal_ */

/* Subroutine */ int cswap_(n, cx, incx, cy, incy)
integer *n;
doublecomplex *cx;
integer *incx;
doublecomplex *cy;
integer *incy;
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    static integer i;
    static doublecomplex ctemp;
    static integer ix, iy;


/*     INTERCHANGES TWO VECTORS. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cy;
    --cx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }

/*       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL */
/*         TO 1 */

    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = ix;
	ctemp.r = cx[i__2].r, ctemp.i = cx[i__2].i;
	i__2 = ix;
	i__3 = iy;
	cx[i__2].r = cy[i__3].r, cx[i__2].i = cy[i__3].i;
	i__2 = iy;
	cy[i__2].r = ctemp.r, cy[i__2].i = ctemp.i;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;

/*       CODE FOR BOTH INCREMENTS EQUAL TO 1 */
L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	i__2 = i;
	ctemp.r = cx[i__2].r, ctemp.i = cx[i__2].i;
	i__2 = i;
	i__3 = i;
	cx[i__2].r = cy[i__3].r, cx[i__2].i = cy[i__3].i;
	i__2 = i;
	cy[i__2].r = ctemp.r, cy[i__2].i = ctemp.i;
/* L30: */
    }
    return 0;
} /* cswap_ */

integer icamax_(n, cx, incx)
integer *n;
doublecomplex *cx;
integer *incx;
{
    /* System generated locals */
    integer ret_val, i__1, i__2;
    doublereal d__1, d__2;

    /* Builtin functions */
    double d_imag();

    /* Local variables */
    static doublereal smax;
    static integer i, ix;


/*     FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cx;

    /* Function Body */
    ret_val = 0;
    if (*n < 1) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        CODE FOR INCREMENT NOT EQUAL TO 1 */

    ix = 1;
    smax = (d__1 = cx[1].r, abs(d__1)) + (d__2 = d_imag(&cx[1]), abs(d__2));
    ix += *incx;
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	i__2 = ix;
	if ((d__1 = cx[i__2].r, abs(d__1)) + (d__2 = d_imag(&cx[ix]), abs(
		d__2)) <= smax) {
	    goto L5;
	}
	ret_val = i;
	i__2 = ix;
	smax = (d__1 = cx[i__2].r, abs(d__1)) + (d__2 = d_imag(&cx[ix]), abs(
		d__2));
L5:
	ix += *incx;
/* L10: */
    }
    return ret_val;

/*        CODE FOR INCREMENT EQUAL TO 1 */

L20:
    smax = (d__1 = cx[1].r, abs(d__1)) + (d__2 = d_imag(&cx[1]), abs(d__2));
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	i__2 = i;
	if ((d__1 = cx[i__2].r, abs(d__1)) + (d__2 = d_imag(&cx[i]), abs(d__2)
		) <= smax) {
	    goto L30;
	}
	ret_val = i;
	i__2 = i;
	smax = (d__1 = cx[i__2].r, abs(d__1)) + (d__2 = d_imag(&cx[i]), abs(
		d__2));
L30:
	;
    }
    return ret_val;
} /* icamax_ */

doublereal scasum_(n, cx, incx)
integer *n;
doublecomplex *cx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1, d__2;

    /* Builtin functions */
    double d_imag();

    /* Local variables */
    static integer i, nincx;
    static doublereal stemp;


/*     TAKES THE SUM OF THE ABSOLUTE VALUES OF A COMPLEX VECTOR AND */
/*     RETURNS A SINGLE PRECISION RESULT. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    /* Parameter adjustments */
    --cx;

    /* Function Body */
    ret_val = 0.;
    stemp = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }

/*        CODE FOR INCREMENT NOT EQUAL TO 1 */

    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = i;
	stemp = stemp + (d__1 = cx[i__3].r, abs(d__1)) + (d__2 = d_imag(&cx[i]
		), abs(d__2));
/* L10: */
    }
    ret_val = stemp;
    return ret_val;

/*        CODE FOR INCREMENT EQUAL TO 1 */

L20:
    i__2 = *n;
    for (i = 1; i <= i__2; ++i) {
	i__1 = i;
	stemp = stemp + (d__1 = cx[i__1].r, abs(d__1)) + (d__2 = d_imag(&cx[i]
		), abs(d__2));
/* L30: */
    }
    ret_val = stemp;
    return ret_val;
} /* scasum_ */

doublereal scnrm2_(n, cx, incx)
integer *n;
doublecomplex *cx;
integer *incx;
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal cutlo = 4.441e-16;
    static doublereal cuthi = 1.304e19;

    /* Format strings */
    static char fmt_30[] = "";
    static char fmt_50[] = "";
    static char fmt_70[] = "";
    static char fmt_90[] = "";
    static char fmt_110[] = "";

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_imag(), sqrt();

    /* Local variables */
    static logical imag;
    static doublereal absx, xmax;
    static integer next, i;
    static logical scale;
    static integer nn;
    static doublereal hitest, sum;

    /* Assigned format variables */
    char *next_fmt;

    /* Parameter adjustments */
    --cx;

    /* Function Body */

/*     UNITARY NORM OF THE COMPLEX N-VECTOR STORED IN CX() WITH STORAGE */
/*     INCREMENT INCX . */
/*     IF    N .LE. 0 RETURN WITH RESULT = 0. */
/*     IF N .GE. 1 THEN INCX MUST BE .GE. 1 */

/*           C.L.LAWSON , 1978 JAN 08 */

/*     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE */
/*     HOPEFULLY APPLICABLE TO ALL MACHINES. */
/*         CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES. */
/*         CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES. */
/*     WHERE */
/*         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1. */
/*         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT) */
/*         V   = LARGEST  NO.            (OVERFLOW  LIMIT) */

/*     BRIEF OUTLINE OF ALGORITHM.. */

/*     PHASE 1    SCANS ZERO COMPONENTS. */
/*     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO */
/*     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO */
/*     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M */
/*     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX. */

/*     VALUES FOR CUTLO AND CUTHI.. */
/*     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER */
/*     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS.. */
/*     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE 
*/
/*                   UNIVAC AND DEC AT 2**(-103) */
/*                   THUS CUTLO = 2**(-51) = 4.44089E-16 */
/*     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC. */
/*                   THUS CUTHI = 2**(63.5) = 1.30438E19 */
/*     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC. */
/*                   THUS CUTLO = 2**(-33.5) = 8.23181D-11 */
/*     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19 */
/*     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 / */
/*     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 / */

    if (*n > 0) {
	goto L10;
    }
    ret_val = zero;
    goto L300;

L10:
    next = 0;
    next_fmt = fmt_30;
    sum = zero;
    nn = *n * *incx;
/*                                                 BEGIN MAIN LOOP */
    i__1 = nn;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	i__3 = i;
	absx = (d__1 = cx[i__3].r, abs(d__1));
	imag = FALSE_;
	switch ((int)next) {
	    case 0: goto L30;
	    case 1: goto L50;
	    case 2: goto L70;
	    case 3: goto L110;
	    case 4: goto L90;
	}
L30:
	if (absx > cutlo) {
	    goto L85;
	}
	next = 1;
	next_fmt = fmt_50;
	scale = FALSE_;

/*                        PHASE 1.  SUM IS ZERO */

L50:
	if (absx == zero) {
	    goto L200;
	}
	if (absx > cutlo) {
	    goto L85;
	}

/*                                PREPARE FOR PHASE 2. */
	next = 2;
	next_fmt = fmt_70;
	goto L105;

/*                                PREPARE FOR PHASE 4. */

L100:
	next = 3;
	next_fmt = fmt_110;
	sum = sum / absx / absx;
L105:
	scale = TRUE_;
	xmax = absx;
	goto L115;

/*                   PHASE 2.  SUM IS SMALL. */
/*                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW. 
*/

L70:
	if (absx > cutlo) {
	    goto L75;
	}

/*                     COMMON CODE FOR PHASES 2 AND 4. */
/*                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFL
OW. */

L110:
	if (absx <= xmax) {
	    goto L115;
	}
/* Computing 2nd power */
	d__1 = xmax / absx;
	sum = one + sum * (d__1 * d__1);
	xmax = absx;
	goto L200;

L115:
/* Computing 2nd power */
	d__1 = absx / xmax;
	sum += d__1 * d__1;
	goto L200;


/*                  PREPARE FOR PHASE 3. */

L75:
	sum = sum * xmax * xmax;

L85:
	next = 4;
	next_fmt = fmt_90;
	scale = FALSE_;

/*     FOR REAL OR D.P. SET HITEST = CUTHI/N */
/*     FOR COMPLEX      SET HITEST = CUTHI/(2*N) */

	hitest = cuthi / (doublereal) (*n);

/*                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING. */

L90:
	if (absx >= hitest) {
	    goto L100;
	}
/* Computing 2nd power */
	d__1 = absx;
	sum += d__1 * d__1;
L200:
/*                  CONTROL SELECTION OF REAL AND IMAGINARY PARTS. */

	if (imag) {
	    goto L210;
	}
	absx = (d__1 = d_imag(&cx[i]), abs(d__1));
	imag = TRUE_;
	switch ((int)next) {
	    case 0: goto L30;
	    case 1: goto L50;
	    case 2: goto L70;
	    case 3: goto L110;
	    case 4: goto L90;
	}

L210:
	;
    }

/*              END OF MAIN LOOP. */
/*              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING. */

    ret_val = sqrt(sum);
    if (scale) {
	ret_val *= xmax;
    }
L300:
    return ret_val;
} /* scnrm2_ */

/* Subroutine */ int scotg_(sa, sb, c, s)
doublereal *sa, *sb, *c, *s;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal r, scale, z, roe;


/*     CONSTRUCT GIVENS PLANE ROTATION. */
/*     JACK DONGARRA, LINPACK, 3/11/78. */


    roe = *sb;
    if (abs(*sa) > abs(*sb)) {
	roe = *sa;
    }
    scale = abs(*sa) + abs(*sb);
    if (scale != 0.) {
	goto L10;
    }
    *c = 1.;
    *s = 0.;
    r = 0.;
    goto L20;
L10:
/* Computing 2nd power */
    d__1 = *sa / scale;
/* Computing 2nd power */
    d__2 = *sb / scale;
    r = scale * sqrt(d__1 * d__1 + d__2 * d__2);
    r = d_sign(&c_b72, &roe) * r;
    *c = *sa / r;
    *s = *sb / r;
L20:
    z = 1.;
    if (abs(*sa) > abs(*sb)) {
	z = *s;
    }
    if (abs(*sb) >= abs(*sa) && *c != 0.) {
	z = 1. / *c;
    }
    *sa = r;
    *sb = z;
    return 0;
} /* scotg_ */

