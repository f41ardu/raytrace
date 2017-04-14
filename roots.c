/* roots.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static doublereal c_b14 = 1e-6;
static integer c__0 = 0;

/* Subroutine */ int laguer_(a, m, x, eps, polish)
doublecomplex *a;
integer *m;
doublecomplex *x;
doublereal *eps;
logical *polish;
{
    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double z_abs();
    void z_div(), z_sqrt();
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer iter;
    static doublecomplex b, d, f, g, h;
    static integer j;
    static doublereal dxold;
    static doublecomplex g2, x1, gm, gp, dx, sq;
    static doublereal abx, cdx, err;

    /* Fortran I/O blocks */
    static cilist io___18 = { 0, 6, 0, 0, 0 };


    /* Parameter adjustments */
    --a;

    /* Function Body */
    dxold = z_abs(x);
    for (iter = 1; iter <= 100; ++iter) {
	i__1 = *m + 1;
	b.r = a[i__1].r, b.i = a[i__1].i;
	err = z_abs(&b);
	d.r = 0., d.i = 0.;
	f.r = 0., f.i = 0.;
	abx = z_abs(x);
	for (j = *m; j >= 1; --j) {
	    z__2.r = x->r * f.r - x->i * f.i, z__2.i = x->r * f.i + x->i * 
		    f.r;
	    z__1.r = z__2.r + d.r, z__1.i = z__2.i + d.i;
	    f.r = z__1.r, f.i = z__1.i;
	    z__2.r = x->r * d.r - x->i * d.i, z__2.i = x->r * d.i + x->i * 
		    d.r;
	    z__1.r = z__2.r + b.r, z__1.i = z__2.i + b.i;
	    d.r = z__1.r, d.i = z__1.i;
	    z__2.r = x->r * b.r - x->i * b.i, z__2.i = x->r * b.i + x->i * 
		    b.r;
	    i__1 = j;
	    z__1.r = z__2.r + a[i__1].r, z__1.i = z__2.i + a[i__1].i;
	    b.r = z__1.r, b.i = z__1.i;
	    err = z_abs(&b) + abx * err;
/* L11: */
	}
	err *= 6e-8;
	if (z_abs(&b) <= err) {
	    dx.r = 0., dx.i = 0.;
	    return 0;
	} else {
	    z_div(&z__1, &d, &b);
	    g.r = z__1.r, g.i = z__1.i;
	    z__1.r = g.r * g.r - g.i * g.i, z__1.i = g.r * g.i + g.i * g.r;
	    g2.r = z__1.r, g2.i = z__1.i;
	    z__3.r = f.r * 2., z__3.i = f.i * 2.;
	    z_div(&z__2, &z__3, &b);
	    z__1.r = g2.r - z__2.r, z__1.i = g2.i - z__2.i;
	    h.r = z__1.r, h.i = z__1.i;
	    d__1 = (doublereal) (*m - 1);
	    d__2 = (doublereal) (*m);
	    z__4.r = d__2 * h.r, z__4.i = d__2 * h.i;
	    z__3.r = z__4.r - g2.r, z__3.i = z__4.i - g2.i;
	    z__2.r = d__1 * z__3.r, z__2.i = d__1 * z__3.i;
	    z_sqrt(&z__1, &z__2);
	    sq.r = z__1.r, sq.i = z__1.i;
	    z__1.r = g.r + sq.r, z__1.i = g.i + sq.i;
	    gp.r = z__1.r, gp.i = z__1.i;
	    z__1.r = g.r - sq.r, z__1.i = g.i - sq.i;
	    gm.r = z__1.r, gm.i = z__1.i;
	    if (z_abs(&gp) < z_abs(&gm)) {
		gp.r = gm.r, gp.i = gm.i;
	    }
	    z__2.r = (doublereal) (*m), z__2.i = 0.;
	    z_div(&z__1, &z__2, &gp);
	    dx.r = z__1.r, dx.i = z__1.i;
	}
	z__1.r = x->r - dx.r, z__1.i = x->i - dx.i;
	x1.r = z__1.r, x1.i = z__1.i;
	if (x->r == x1.r && x->i == x1.i) {
	    return 0;
	}
	x->r = x1.r, x->i = x1.i;
	cdx = z_abs(&dx);
	if (iter > 6 && cdx >= dxold) {
	    return 0;
	}
	dxold = cdx;
	if (! (*polish)) {
	    if (z_abs(&dx) <= *eps * z_abs(x)) {
		return 0;
	    }
	}
/* L12: */
    }
    s_wsle(&io___18);
    do_lio(&c__9, &c__1, "too many iterations", 19L);
    e_wsle();
    return 0;
} /* laguer_ */


/* Subroutine */ int zroots_(a, m, roots, polish)
doublecomplex *a;
integer *m;
doublecomplex *roots;
logical *polish;
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1, d__2, d__3;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double d_imag();

    /* Local variables */
    static doublecomplex b, c;
    static integer i, j;
    static doublecomplex x, ad[101];
    static integer jj;
    extern /* Subroutine */ int laguer_();

    /* Parameter adjustments */
    --roots;
    --a;

    /* Function Body */
    i__1 = *m + 1;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j - 1;
	i__3 = j;
	ad[i__2].r = a[i__3].r, ad[i__2].i = a[i__3].i;
/* L11: */
    }
    for (j = *m; j >= 1; --j) {
	x.r = 0., x.i = 0.;
	laguer_(ad, &j, &x, &c_b14, &c__0);
	if ((d__1 = d_imag(&x), abs(d__1)) <= (d__2 = x.r, abs(d__2)) * 2e-12)
		 {
	    d__3 = x.r;
	    z__1.r = d__3, z__1.i = 0.;
	    x.r = z__1.r, x.i = z__1.i;
	}
	i__1 = j;
	roots[i__1].r = x.r, roots[i__1].i = x.i;
	i__1 = j;
	b.r = ad[i__1].r, b.i = ad[i__1].i;
	for (jj = j; jj >= 1; --jj) {
	    i__1 = jj - 1;
	    c.r = ad[i__1].r, c.i = ad[i__1].i;
	    i__1 = jj - 1;
	    ad[i__1].r = b.r, ad[i__1].i = b.i;
	    z__2.r = x.r * b.r - x.i * b.i, z__2.i = x.r * b.i + x.i * b.r;
	    z__1.r = z__2.r + c.r, z__1.i = z__2.i + c.i;
	    b.r = z__1.r, b.i = z__1.i;
/* L12: */
	}
/* L13: */
    }
    if (*polish) {
	i__1 = *m;
	for (j = 1; j <= i__1; ++j) {
	    laguer_(&a[1], m, &roots[j], &c_b14, &c__1);
/* L14: */
	}
    }
    i__1 = *m;
    for (j = 2; j <= i__1; ++j) {
	i__2 = j;
	x.r = roots[i__2].r, x.i = roots[i__2].i;
	for (i = j - 1; i >= 1; --i) {
	    i__2 = i;
	    if (roots[i__2].r <= x.r) {
		goto L10;
	    }
	    i__2 = i + 1;
	    i__3 = i;
	    roots[i__2].r = roots[i__3].r, roots[i__2].i = roots[i__3].i;
/* L15: */
	}
	i = 0;
L10:
	i__2 = i + 1;
	roots[i__2].r = x.r, roots[i__2].i = x.i;
/* L16: */
    }
    return 0;
} /* zroots_ */

