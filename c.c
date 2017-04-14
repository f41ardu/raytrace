/* c.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal en[100], fp[100], fpp[100], fps[100], fpps[100];
} transfer_;

#define transfer_1 transfer_

/* Table of constant values */

static doublereal c_b10 = 293.5;
static integer c__20 = 20;
static doublereal c_b13 = 2e30;
static integer c__5 = 5;
static integer c__1 = 1;
static integer c__9 = 9;
static integer c__100 = 100;


/* Subroutine */ int strfac_(h, k, l, alamda, psi, imat)
doublereal *h, *k, *l, *alamda;
doublecomplex *psi;
integer *imat;
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
    doublecomplex z__1, z__2, z__3, z__4, z__5, z__6, z__7, z__8, z__9, z__10,
	     z__11, z__12, z__13, z__14, z__15, z__16, z__17, z__18, z__19, 
	    z__20, z__21, z__22, z__23, z__24, z__25, z__26, z__27, z__28, 
	    z__29, z__30, z__31;

    /* Builtin functions */
    double atan();
    void z_exp();
    double z_abs();

    /* Local variables */
    static doublereal gfak;
    static doublecomplex fhkl;
    static doublereal f, astar, dh, fp, vc, pi;
    static doublecomplex sk;
    extern /* Subroutine */ int debwal_();
    static doublereal rcl, fpp, zpi;
    extern /* Subroutine */ int atomfor_();


/* --- fhkl fuer si */


    pi = atan(1.) * 4.;
    zpi = pi * 2.;

    z__10.r = zpi * 0., z__10.i = zpi * 1.;
    d__1 = *h * 0. + *k * 0. + *l * 0.;
    z__9.r = d__1 * z__10.r, z__9.i = d__1 * z__10.i;
    z_exp(&z__8, &z__9);
    z__13.r = zpi * 0., z__13.i = zpi * 1.;
    d__2 = *h * 0. + *k * .5 + *l * .5;
    z__12.r = d__2 * z__13.r, z__12.i = d__2 * z__13.i;
    z_exp(&z__11, &z__12);
    z__7.r = z__8.r + z__11.r, z__7.i = z__8.i + z__11.i;
    z__16.r = zpi * 0., z__16.i = zpi * 1.;
    d__3 = *h * .5 + *k * 0. + *l * .5;
    z__15.r = d__3 * z__16.r, z__15.i = d__3 * z__16.i;
    z_exp(&z__14, &z__15);
    z__6.r = z__7.r + z__14.r, z__6.i = z__7.i + z__14.i;
    z__19.r = zpi * 0., z__19.i = zpi * 1.;
    d__4 = *h * .5 + *k * .5 + *l * 0.;
    z__18.r = d__4 * z__19.r, z__18.i = d__4 * z__19.i;
    z_exp(&z__17, &z__18);
    z__5.r = z__6.r + z__17.r, z__5.i = z__6.i + z__17.i;
    z__22.r = zpi * 0., z__22.i = zpi * 1.;
    d__5 = *h * .25 + *k * .25 + *l * .25;
    z__21.r = d__5 * z__22.r, z__21.i = d__5 * z__22.i;
    z_exp(&z__20, &z__21);
    z__4.r = z__5.r + z__20.r, z__4.i = z__5.i + z__20.i;
    z__25.r = zpi * 0., z__25.i = zpi * 1.;
    d__6 = *h * .25 + *k * .75 + *l * .75;
    z__24.r = d__6 * z__25.r, z__24.i = d__6 * z__25.i;
    z_exp(&z__23, &z__24);
    z__3.r = z__4.r + z__23.r, z__3.i = z__4.i + z__23.i;
    z__28.r = zpi * 0., z__28.i = zpi * 1.;
    d__7 = *h * .75 + *k * .25 + *l * .75;
    z__27.r = d__7 * z__28.r, z__27.i = d__7 * z__28.i;
    z_exp(&z__26, &z__27);
    z__2.r = z__3.r + z__26.r, z__2.i = z__3.i + z__26.i;
    z__31.r = zpi * 0., z__31.i = zpi * 1.;
    d__8 = *h * .75 + *k * .75 + *l * .25;
    z__30.r = d__8 * z__31.r, z__30.i = d__8 * z__31.i;
    z_exp(&z__29, &z__30);
    z__1.r = z__2.r + z__29.r, z__1.i = z__2.i + z__29.i;
    sk.r = z__1.r, sk.i = z__1.i;
    gfak = z_abs(&sk);
    if (*h + *k + *l != 0.) {
	debwal_(h, k, l, &c_b10, &dh, imat);
    } else {
	dh = 1.;
    }
    if (*imat == 1) {
	astar = 5.43102018;
    }
    if (*imat == 2) {
	astar = 3.576;
    }
    if (*imat == 3) {
	astar = 5.658;
    }
/* Computing 3rd power */
    d__1 = astar, d__2 = d__1;
    vc = d__2 * (d__1 * d__1);
    rcl = 2.81774e-5;

    atomfor_(h, k, l, alamda, &f, &fp, &fpp, imat);

    d__1 = f + fp;
    z__2.r = d__1, z__2.i = fpp;
    z__1.r = dh * z__2.r, z__1.i = dh * z__2.i;
    fhkl.r = z__1.r, fhkl.i = z__1.i;

/* Computing 2nd power */
    d__2 = *alamda;
    d__1 = -(rcl * (d__2 * d__2) / (pi * vc));
    z__2.r = d__1 * fhkl.r, z__2.i = d__1 * fhkl.i;
    z__1.r = gfak * z__2.r, z__1.i = gfak * z__2.i;
    psi->r = z__1.r, psi->i = z__1.i;
    return 0;
} /* strfac_ */


/* Subroutine */ int atomfor_(h, k, l, alamda, f, fp, fpp, imat)
doublereal *h, *k, *l, *alamda, *f, *fp, *fpp;
integer *imat;
{
    /* Initialized data */

    static doublereal a0 = 5.43102018;
    static doublereal a1 = 3.576;
    static doublereal a2 = 5.658;
    static doublereal thovlm[20] = { 0.,.05,.1,.15,.2,.25,.3,.35,.4,.5,.6,.7,
	    .8,.9,1.,1.1,1.2,1.3,1.4,1.5 };
    static doublereal matscf[20] = { 14.,13.45,12.16,10.79,9.67,8.84,8.22,7.7,
	    7.2,6.24,5.31,4.47,3.75,3.16,2.96,2.35,2.07,1.87,1.71,1.6 };
    static doublereal mge[20] = { 32.,31.28,29.52,27.48,25.53,23.76,22.11,
	    20.54,19.02,16.19,13.72,11.68,10.08,8.87,7.96,7.29,6.77,6.37,6.02,
	    5.72 };
    static doublereal thc[20] = { 0.,.05,.1,.15,.2,.25,.3,.35,.4,.5,.6,.7,.8,
	    .9,1.,1.1,1.3,1.5,1.7,1.9 };
    static doublereal mc[20] = { 6.,5.76,5.126,4.358,3.581,2.976,2.502,2.165,
	    1.95,1.685,1.536,1.426,1.322,1.218,1.114,1.012,.821,.659,.524,
	    .419 };

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal y_spline__[20];
    extern /* Subroutine */ int fdata_();
    static doublereal x;
    static integer ih, ik, il;
    extern /* Subroutine */ int spline_(), splint_();
    static doublereal hkl;


/* --- Interpolation Atomarer Streufaktoren f,fp,fpp */
/*     fuer Silizium */
/*     fp, fpp sind mit absorbtion auf der DesyVax ermittelt */
/*     Nach Cromer - Liebermann */
/*     Die Daten fuer f sind aus den International Tables of */
/*     X-Ray Diffraction Kap. 3.3 ff */

/*     Die fuer f' und f'' sind auf der DESY Vax mit */
/*     den Routinen zum Packet Absorption bestimmt. */

/* Gi Kn Silizium */
/* Kohlenstoff */
/* Germanium */

/*      write(6,*) ' atomfor ..' */
    if (*imat == 1) {
	spline_(thovlm, matscf, &c__20, &c_b13, &c_b13, y_spline__);
    }
    if (*imat == 2) {
	spline_(thc, mc, &c__20, &c_b13, &c_b13, y_spline__);
    }
    if (*imat == 3) {
	spline_(thovlm, mge, &c__20, &c_b13, &c_b13, y_spline__);
    }

    ih = (integer) (*h);
    ik = (integer) (*k);
    il = (integer) (*l);
    hkl = *h * *h + *k * *k + *l * *l;
    if (*imat == 1) {
	x = sqrt(hkl) / (a0 * 2.);
    }
    if (*imat == 2) {
	x = sqrt(hkl) / (a1 * 2.);
    }
    if (*imat == 3) {
	x = sqrt(hkl) / (a2 * 2.);
    }

    if (*imat == 1) {
	splint_(thovlm, matscf, y_spline__, &c__20, &x, f);
    }
    if (*imat == 2) {
	splint_(thc, mc, y_spline__, &c__20, &x, f);
    }
    if (*imat == 3) {
	splint_(thovlm, mge, y_spline__, &c__20, &x, f);
    }

/* wird im hauptprogramm aufgerufen (1. mal) */
/* der bereich von 100 eV um die exacte Energie sollte */
/* ausreichen */
/*      if(hkl.ne.0.) call fspline(12349./alamda,imat) */

    d__1 = 12349. / *alamda;
    fdata_(&d__1, fp, fpp);

    return 0;
} /* atomfor_ */

/* c                                      ++ */
/* Subroutine */ int fspline_(energy, imat)
doublereal *energy;
integer *imat;
{
    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;
    alist al__1;

    /* Builtin functions */
    integer f_open(), f_rew(), s_rsle(), do_lio(), e_rsle(), f_clos(), s_wsle(
	    ), e_wsle();

    /* Local variables */
    static doublereal rdum;
    static integer i, ndata;
    extern /* Subroutine */ int spline_();
    static doublereal entest, testen;

    /* Fortran I/O blocks */
    static cilist io___28 = { 0, 11, 1, 0, 0 };
    static cilist io___32 = { 1, 11, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, 0, 0 };



/* --- berechnung von 100 spline koeffizienten in der neahe der */
/*     gesuchten energie */
/*     energy muss durch 5 teilbar sein */

/*      /dosc/fprime/fppp14.dat */
    if (*imat == 1) {
	o__1.oerr = 0;
	o__1.ounit = 11;
	o__1.ofnmlen = 23;
	o__1.ofnm = "/dosc/fprime/fppp14.dat";
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
    }
    if (*imat == 2) {
	o__1.oerr = 0;
	o__1.ounit = 11;
	o__1.ofnmlen = 22;
	o__1.ofnm = "/dosc/fprime/fppp6.dat";
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
    }
    if (*imat == 3) {
	o__1.oerr = 0;
	o__1.ounit = 11;
	o__1.ofnmlen = 23;
	o__1.ofnm = "/dosc/fprime/fppp32.dat";
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
    }
/*      if(imat .eq. 1) then */
/*      open(11,file='/home/thomas_old/fprime/fppp14.dat') */
/*      endif */
/*      if(imat .eq. 2) then */
/*      open(11,file='/home/thomas_old/fprime/fppp6.dat') */
/*      endif */
/*      if(imat .eq. 3) then */
/*      open(11,file='/home/thomas_old/fprime/fppp32.dat') */
/*      endif */

/* --- einlesen bis zu einer energie 50*5 eV unterhalb der */
/*     gewuenschten energie */

    if (*imat == 1) {
	*energy = ((integer) (*energy / 5.) + 1.) * 5.;
	entest = *energy - 250.;
    }
    if (*imat == 2 || *imat == 3) {
	*energy = ((integer) (*energy / 10.) + 1.) * 10.;
	entest = *energy - 500.;
    }
    al__1.aerr = 0;
    al__1.aunit = 11;
    f_rew(&al__1);
L5:
    i__1 = s_rsle(&io___28);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&testen, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&rdum, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&rdum, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = e_rsle();
    if (testen != entest) {
	goto L5;
    }

/* --- dann die naechsten hundert Werte fuer die Tabelle */
/*     einlesen */

    i = 0;
L10:
    ++i;
    i__1 = s_rsle(&io___32);
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&transfer_1.en[i - 1], (ftnlen)sizeof(
	    doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&transfer_1.fp[i - 1], (ftnlen)sizeof(
	    doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&transfer_1.fpp[i - 1], (ftnlen)
	    sizeof(doublereal));
    if (i__1 != 0) {
	goto L100;
    }
    i__1 = e_rsle();
    if (i != 100) {
	goto L10;
    }
    ndata = i;

/* --- spline koeffizienten bestimmen */

    spline_(transfer_1.en, transfer_1.fp, &ndata, &c_b13, &c_b13, 
	    transfer_1.fps);
    spline_(transfer_1.en, transfer_1.fpp, &ndata, &c_b13, &c_b13, 
	    transfer_1.fpps);

    cl__1.cerr = 0;
    cl__1.cunit = 11;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
L100:
    s_wsle(&io___34);
    do_lio(&c__9, &c__1, "Ein Fehler in fspline(energy) ...", 33L);
    e_wsle();
    return 0;
} /* fspline_ */

/* c */
/* Subroutine */ int fdata_(energy, fpo, fppo)
doublereal *energy, *fpo, *fppo;
{
    extern /* Subroutine */ int splint_();

/*     ermittelt mit splint werte fuer fp,fpp aus */
/* --- tabellierten werten von fp, fpp fuer si */
/*     ermittelt mit absorbtion auf vxdesy im bereich 250 ~ 10000 eV */



/* --- fp und fpp mit splint berechnen */

    splint_(transfer_1.en, transfer_1.fp, transfer_1.fps, &c__100, energy, 
	    fpo);
    splint_(transfer_1.en, transfer_1.fpp, transfer_1.fpps, &c__100, energy, 
	    fppo);

    return 0;
} /* fdata_ */


/* --- aus Numerical Recipes  Press et all Cambridge University Press 1989 */

/* Subroutine */ int splint_(xa, ya, y2a, n, x, y)
doublereal *xa, *ya, *y2a;
integer *n;
doublereal *x, *y;
{
    /* System generated locals */
    doublereal d__1, d__2, d__3, d__4, d__5;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static doublereal a, b, h;
    static integer k, khi, klo;

    /* Fortran I/O blocks */
    static cilist io___39 = { 0, 6, 0, 0, 0 };


    /* Parameter adjustments */
    --y2a;
    --ya;
    --xa;

    /* Function Body */
    klo = 1;
    khi = *n;
L1:
    if (khi - klo > 1) {
	k = (khi + klo) / 2;
	if (xa[k] > *x) {
	    khi = k;
	} else {
	    klo = k;
	}
	goto L1;
    }
    h = xa[khi] - xa[klo];
    if (h == 0.) {
	s_wsle(&io___39);
	do_lio(&c__9, &c__1, "Bad XA input.", 13L);
	e_wsle();
	return 0;
    }
    a = (xa[khi] - *x) / h;
    b = (*x - xa[klo]) / h;
/* Computing 3rd power */
    d__1 = a, d__2 = d__1;
/* Computing 3rd power */
    d__3 = b, d__4 = d__3;
/* Computing 2nd power */
    d__5 = h;
    *y = a * ya[klo] + b * ya[khi] + ((d__2 * (d__1 * d__1) - a) * y2a[klo] + 
	    (d__4 * (d__3 * d__3) - b) * y2a[khi]) * (d__5 * d__5) / 6.;
    return 0;
} /* splint_ */


/* --- Spline */

/* Subroutine */ int spline_(x, y, n, yp1, ypn, y2)
doublereal *x, *y;
integer *n;
doublereal *yp1, *ypn, *y2;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, k;
    static doublereal p, u[100], qn, un, sig;

    /* Parameter adjustments */
    --y2;
    --y;
    --x;

    /* Function Body */
    if (*yp1 > 9.9e29) {
	y2[1] = 0.;
	u[0] = 0.;
    } else {
	y2[1] = -.5;
	u[0] = 3. / (x[2] - x[1]) * ((y[2] - y[1]) / (x[2] - x[1]) - *yp1);
    }
    i__1 = *n - 1;
    for (i = 2; i <= i__1; ++i) {
	sig = (x[i] - x[i - 1]) / (x[i + 1] - x[i - 1]);
	p = sig * y2[i - 1] + 2.;
	y2[i] = (sig - 1.) / p;
	u[i - 1] = (((y[i + 1] - y[i]) / (x[i + 1] - x[i]) - (y[i] - y[i - 1])
		 / (x[i] - x[i - 1])) * 6. / (x[i + 1] - x[i - 1]) - sig * u[
		i - 2]) / p;
/* L11: */
    }
    if (*ypn > 9.9e29) {
	qn = 0.;
	un = 0.;
    } else {
	qn = .5;
	un = 3. / (x[*n] - x[*n - 1]) * (*ypn - (y[*n] - y[*n - 1]) / (x[*n] 
		- x[*n - 1]));
    }
    y2[*n] = (un - qn * u[*n - 2]) / (qn * y2[*n - 1] + 1.);
    for (k = *n - 1; k >= 1; --k) {
	y2[k] = y2[k] * y2[k + 1] + u[k - 1];
/* L12: */
    }
    return 0;
} /* spline_ */

