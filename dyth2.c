/* dyth2.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 0.;
static integer c__2 = 2;
static integer c__1 = 1;

doublereal reflex_(h, k, l, lambda, dt_geom__, asy, laue_bragg__, dicke)
doublereal *h, *k, *l, *lambda, *dt_geom__, *asy;
integer *laue_bragg__;
doublereal *dicke;
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3, d__4;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6;

    /* Builtin functions */
    double atan(), cos(), sin();
    void pow_zi(), z_div();
    double z_abs();

    /* Local variables */
    static doublecomplex chih;
    static integer imat;
    static doublecomplex a[3];
    static doublereal b, alpha;
    static doublecomplex z;
    static logical sigma;
    static doublereal theta;
    static doublecomplex c1, c2, d0;
    static doublereal k0, gamma0, t1;
    static doublecomplex roots[2], x1, x2, d01, d02, dh;
    static doublereal dp, pi, gammah;
    extern doublereal gtheta_();
    extern /* Subroutine */ int strfac_(), laueit_(), zroots_();
    static doublereal ibh;
    static logical par;
    static doublereal pol;
    extern /* Subroutine */ int braggit_(), me_cexp__();
    static doublecomplex chi0, phi1, phi2;



/* --- Berechnung der Reflektionskurven */
/*     im Zweistrahlfall nach den im Zachariasen */
/*     angegebenen Gleichungen (modifiziert) */





    pi = atan(1.) * 4.;

/* --- eingabe parmeter */

    imat = 1;
/*      asy = 0. */
    t1 = *dicke;
    sigma = TRUE_;
/*      h = 2 */
/*      k = 2 */
/*      l = 0 */
    d__1 = *h;
    d__2 = *k;
    d__3 = *l;
    d__4 = *lambda;
    strfac_(&d__1, &d__2, &d__3, &d__4, &chih, &imat);
    d__1 = *lambda;
    strfac_(&c_b2, &c_b2, &c_b2, &d__1, &chi0, &imat);

    k0 = 1. / (*lambda * 1e-7);

    theta = gtheta_(h, k, l, lambda, &imat);

    *asy = *asy * pi / 180.;

/* Laue */

    if (*laue_bragg__ == 1) {
	gamma0 = cos(theta - *asy);
	gammah = cos(theta + *asy);
    }
/* Bragg */
    if (*laue_bragg__ == 2) {
	gamma0 = sin(theta + *asy);
	gammah = -sin(theta - *asy);
    }

    b = gamma0 / gammah;

    if (par) {
	pol = cos(theta * 2.);
/* paralell Pol */
    }

    if (sigma) {
	pol = 1.;
    }

    dp = theta - *dt_geom__;
    alpha = dp * 2. * sin(theta * 2.);
    d__1 = 1. - b;
    z__3.r = d__1 * chi0.r, z__3.i = d__1 * chi0.i;
    z__2.r = z__3.r / 2., z__2.i = z__3.i / 2.;
    d__2 = b / 2. * alpha;
    z__1.r = z__2.r + d__2, z__1.i = z__2.i;
    z.r = z__1.r, z.i = z__1.i;
    pow_zi(&z__3, &chi0, &c__2);
    z__4.r = alpha * chi0.r, z__4.i = alpha * chi0.i;
    z__2.r = z__3.r - z__4.r, z__2.i = z__3.i - z__4.i;
    z__6.r = pol * chih.r, z__6.i = pol * chih.i;
    pow_zi(&z__5, &z__6, &c__2);
    z__1.r = z__2.r - z__5.r, z__1.i = z__2.i - z__5.i;
    a[0].r = z__1.r, a[0].i = z__1.i;
    d__1 = alpha * 2.;
    z__3.r = d__1, z__3.i = 0.;
    z__5.r = chi0.r * 2., z__5.i = chi0.i * 2.;
    z__4.r = z__5.r / b, z__4.i = z__5.i / b;
    z__2.r = z__3.r - z__4.r, z__2.i = z__3.i - z__4.i;
    z__6.r = chi0.r * 2., z__6.i = chi0.i * 2.;
    z__1.r = z__2.r - z__6.r, z__1.i = z__2.i - z__6.i;
    a[1].r = z__1.r, a[1].i = z__1.i;
    d__1 = 4. / b;
    z__1.r = d__1, z__1.i = 0.;
    a[2].r = z__1.r, a[2].i = z__1.i;
    zroots_(a, &c__2, roots, &c__1);
    d01.r = roots[0].r, d01.i = roots[0].i;
    d02.r = roots[1].r, d02.i = roots[1].i;
    z__3.r = d01.r * 2., z__3.i = d01.i * 2.;
    z__2.r = z__3.r - chi0.r, z__2.i = z__3.i - chi0.i;
    z__4.r = pol * chih.r, z__4.i = pol * chih.i;
    z_div(&z__1, &z__2, &z__4);
    x1.r = z__1.r, x1.i = z__1.i;
    z__3.r = d02.r * 2., z__3.i = d02.i * 2.;
    z__2.r = z__3.r - chi0.r, z__2.i = z__3.i - chi0.i;
    z__4.r = pol * chih.r, z__4.i = pol * chih.i;
    z_div(&z__1, &z__2, &z__4);
    x2.r = z__1.r, x2.i = z__1.i;
    d__1 = pi * 2. * t1 * k0;
    z__2.r = d__1 * d01.r, z__2.i = d__1 * d01.i;
    z__1.r = z__2.r / gamma0, z__1.i = z__2.i / gamma0;
    phi1.r = z__1.r, phi1.i = z__1.i;
    d__1 = pi * 2. * t1 * k0;
    z__2.r = d__1 * d02.r, z__2.i = d__1 * d02.i;
    z__1.r = z__2.r / gamma0, z__1.i = z__2.i / gamma0;
    phi2.r = z__1.r, phi2.i = z__1.i;
    z__1.r = phi1.r * 0. - phi1.i * -1., z__1.i = phi1.r * -1. + phi1.i * 0.;
    me_cexp__(&c1, &z__1);
    z__1.r = phi2.r * 0. - phi2.i * -1., z__1.i = phi2.r * -1. + phi2.i * 0.;
    me_cexp__(&c2, &z__1);

    if (*laue_bragg__ == 1) {
	laueit_(&x1, &x2, &c1, &c2, &d0, &dh);
    }
    if (*laue_bragg__ == 2) {
	braggit_(&x1, &x2, &c1, &c2, &d0, &dh);
    }

/* Computing 2nd power */
    d__1 = z_abs(&dh);
    ibh = d__1 * d__1 / abs(b);
/*         IB0 = (CABS(D0)**2)/abs(b) */
    ret_val = ibh;

    return ret_val;
} /* reflex_ */


/* Subroutine */ int me_cexp__(result, arg)
doublecomplex *result, *arg;
{
    /* System generated locals */
    doublereal d__1;
    doublecomplex z__1, z__2;

    /* Builtin functions */
    double atan(), d_imag(), d_mod(), exp(), cos(), sin();

    /* Local variables */
    static doublereal iarg, rarg, emuet, pi, resultiarg, resultrarg;


/* --- Zerlegung der complexen Exponentialfunktion */
/*    Auswertung der trignometischen Funktionen ueber */
/*    Hauptwerte */


    pi = atan(1.) * 4.;

    result->r = 0., result->i = 0.;

    rarg = arg->r;
    iarg = d_imag(arg);

    d__1 = pi * 2.;
    iarg = d_mod(&iarg, &d__1);

    if (rarg >= 0.) {
	if (rarg >= 20.) {
	    emuet = exp(20.);
	} else {
	    emuet = exp(rarg);
	}
    }

    if (rarg < 0.) {
	if (rarg <= -20.) {
	    emuet = exp(-20.);
	} else {
	    emuet = exp(rarg);
	}
    }

    resultrarg = cos(iarg);
    resultiarg = sin(iarg);

    z__2.r = resultrarg, z__2.i = resultiarg;
    z__1.r = emuet * z__2.r, z__1.i = emuet * z__2.i;
    result->r = z__1.r, result->i = z__1.i;
    return 0;

} /* me_cexp__ */

