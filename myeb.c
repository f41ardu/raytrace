/* myeb.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__14 = 14;
static integer c__6 = 6;
static integer c__32 = 32;

/* Subroutine */ int debwal_(h, k, l, temp, dh, istoff)
doublereal *h, *k, *l, *temp, *dh;
integer *istoff;
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal d, atomm;
    extern /* Subroutine */ int debyew_();
    static doublereal hkl, debtemp;
    extern doublereal debyetp_();


/*   Bestimmung von Wellenlaenge,Braggwinkel,s=SIN(thb)/lambda */
/*    Debye-W.-Faktor dh, Atommasse atomm [u], Gitterkonstante d [A] : */

    if (*istoff == 1) {
	d = 5.43102018;
	atomm = 28.086;
    }
    if (*istoff == 2) {
	d = 3.567;
	atomm = 12.01;
    }
    if (*istoff == 3) {
	d = 5.658;
	atomm = 72.59;
    }
    hkl = sqrt(*h * *h + *k * *k + *l * *l);
/* germanium      atomm=72.59 */
/* Atommasse in 1.E-27 [kg] : */
    atomm *= 1.6605655;
    if (*istoff == 1) {
	debtemp = debyetp_(&c__14);
    }
    if (*istoff == 2) {
	debtemp = debyetp_(&c__6);
    }
    if (*istoff == 3) {
	debtemp = debyetp_(&c__32);
    }

    d__1 = d / hkl;
    debyew_(&d__1, &debtemp, &atomm, temp, dh);


    return 0;
} /* debwal_ */


/* Subroutine */ int debyew_(d, td, am, to, dh)
doublereal *d, *td, *am, *to, *dh;
{
    /* Builtin functions */
    double atan(), exp();

    /* Local variables */
    static doublereal h, m, x, kb, pi;
    extern doublereal phi_();
    static doublereal sum;

/* ---------------------------------------------------------------------C 
*/
/*                                                                     C 
*/
/* DEBYEWaller berechnet den D.-W.-Faktor dh, siehe auch PHI.SUB  .    C 
*/
/*   d : Netzebenenabstand ;        [ Angstroem ] = 1.E-10 [ m ]       C 
*/
/*  td : Debyetemperatur ;          [ K ]                              C 
*/
/*  to : Temperatur ;               [ K ]                              C 
*/
/*  am : Atommasse ;         1.E-27 [ kg ]                             C 
*/
/*  dh : Debye-Waller-Faktor ;      [ dimensionslos ]                  C 
*/
/*                                                                     C 
*/
/* ---------------------------------------------------------------------C 
*/
/*   Festlegung der Konstanten : */
    pi = atan(1.) * 4.;
    h = 6.626196;
/*    Wirkungsquantum * 1.E34 */
    kb = 1.380622;
/*    Boltzmann-Konst. * 1.E23 */

    x = *td / *to;
    sum = x * .25 + phi_(&x);
    m = h * 6. * h * *to / (*am * kb * *td * *td) * sum;
    *dh = exp(-m / (*d * 4. * *d) * 100.);
/* Faktor 100 durch die Konstanten ! */
/* L20: */
    return 0;
} /* debyew_ */


doublereal debyetp_(imat)
integer *imat;
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal dt;

/* ---------------------------------------------------------------------C 
*/
/* Vers. Th. Rautenstrauch                                             C 
*/
/* Lit.: Salter,Adv.Phys. 14,1(1965) , Aldred & Hart (1973),           C 
*/
/*       Batterman & Chipman,Phys.Rev.127,690(1962)                    C 
*/
/*                                                                     C 
*/
/* ---------------------------------------------------------------------C 
*/
    if (*imat == 14) {
	dt = 542.5;
    } else if (*imat == 32) {
	dt = 295.;
    } else if (*imat == 6) {
	dt = 1860.;
/* 2230. (Kittel) */
    }

    ret_val = dt;
    return ret_val;
} /* debyetp_ */


doublereal phi_(z)
doublereal *z;
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double atan(), sqrt(), exp(), log();

    /* Local variables */
    static doublereal a, e, s, u, y, en, pi;

/* ------------------------------------------------------------------C */
/* Modifiziert Thomas Rautenstrauch 19/02/93                        C */
/*    PHI(z) berechnet fuer den Debye-Waller Faktor die  Groesse    C */
/*    (1/z)*(Integral von 0...z ueber x/(EXP(x)-1)*dx)              C */
/*    mit z = Debyetemperatur/Temperatur                            C */
/* ------------------------------------------------------------------C */
    pi = atan(1.) * 4.;
    if (*z <= .08) {
	y = sqrt(.6) * 4. * atan(*z / sqrt(.6) / (*z + 4.));
    } else if (*z > 30.) {
	y = pi * pi / 6.;
    } else {
	u = 1.;
	e = exp(-(*z));
	en = e;
	s = e;
L99:
	u += 1.;
	en *= e;
	a = en / (u * u);
	s += a;
	if (a * 1e8 > s) {
	    goto L99;
	}
	y = pi * pi / 6. + *z * log(1. - exp(-(*z))) - s;
    }
/* L999: */
    if (*z != 0.) {
	y /= *z;
    } else {
	y = 1.;
    }
    ret_val = y;
    return ret_val;
} /* phi_ */

