/* raytools.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

doublereal skalar_produkt__(vektor1, vektor2)
doublereal *vektor1, *vektor2;
{
    /* System generated locals */
    doublereal ret_val;


/* ---   vektor skalarproduk */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektor2;
    --vektor1;

    /* Function Body */
    ret_val = 0.;
    ret_val = vektor1[1] * vektor2[1] + vektor1[2] * vektor2[2] + vektor1[3] *
	     vektor2[3];
    return ret_val;
} /* skalar_produkt__ */


/* Subroutine */ int vektor_addition__(vektor1, vektor2, vektorout)
doublereal *vektor1, *vektor2, *vektorout;
{
    static integer iloop;


/* ---   vektor addition */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektorout;
    --vektor2;
    --vektor1;

    /* Function Body */
    for (iloop = 1; iloop <= 3; ++iloop) {
	vektorout[iloop] = vektor1[iloop] + vektor2[iloop];
    }
    return 0;
} /* vektor_addition__ */


/* Subroutine */ int vektor_subtraktion__(vektor1, vektor2, vektorout)
doublereal *vektor1, *vektor2, *vektorout;
{
    static integer iloop;


/* ---   vektor subtraktion */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektorout;
    --vektor2;
    --vektor1;

    /* Function Body */
    for (iloop = 1; iloop <= 3; ++iloop) {
	vektorout[iloop] = vektor1[iloop] - vektor2[iloop];
    }
    return 0;
} /* vektor_subtraktion__ */


/* Subroutine */ int vektor_kreuzprodukt__(vektor1, vektor2, vektorout)
doublereal *vektor1, *vektor2, *vektorout;
{

/* ---   vektor kreuzprodukt */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektorout;
    --vektor2;
    --vektor1;

    /* Function Body */
    vektorout[1] = vektor1[2] * vektor2[3] - vektor1[3] * vektor2[2];
    vektorout[2] = vektor1[3] * vektor2[1] - vektor1[1] * vektor2[3];
    vektorout[3] = vektor1[1] * vektor2[2] - vektor1[2] * vektor2[1];
    return 0;
} /* vektor_kreuzprodukt__ */


/* Subroutine */ int vektor_mit_skalar__(skalar, vektorin, vektorout)
doublereal *skalar, *vektorin, *vektorout;
{
    static integer iloop;


/* ---   multilpikation eines Vektors mit einem Skalar */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektorout;
    --vektorin;

    /* Function Body */
    for (iloop = 1; iloop <= 3; ++iloop) {
	vektorout[iloop] = *skalar * vektorin[iloop];
    }
    return 0;
} /* vektor_mit_skalar__ */


doublereal vektor_laenge__(vektor)
doublereal *vektor;
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt();


/* --- Laenge eines Vektors */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektor;

    /* Function Body */
/* Computing 2nd power */
    d__1 = vektor[1];
/* Computing 2nd power */
    d__2 = vektor[2];
/* Computing 2nd power */
    d__3 = vektor[3];
    ret_val = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
    return ret_val;
} /* vektor_laenge__ */


/* Subroutine */ int vektor_norm__(vektor, norm)
doublereal *vektor, *norm;
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer iloop;
    static doublereal vektor_laenge__;


/* ---   normierter Vektor */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --norm;
    --vektor;

    /* Function Body */
/* Computing 2nd power */
    d__1 = vektor[1];
/* Computing 2nd power */
    d__2 = vektor[2];
/* Computing 2nd power */
    d__3 = vektor[3];
    vektor_laenge__ = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
    for (iloop = 1; iloop <= 3; ++iloop) {
	norm[iloop] = vektor[iloop] / vektor_laenge__;
    }
    return 0;
} /* vektor_norm__ */


doublereal winkel_(vektor1, vektor2)
doublereal *vektor1, *vektor2;
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(), acos();

    /* Local variables */
    static doublereal v1, v2;


/* --- Winkel zwischen den Vektoren w,v uebers Skalarprodukt */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektor2;
    --vektor1;

    /* Function Body */
/* Computing 2nd power */
    d__1 = vektor1[1];
/* Computing 2nd power */
    d__2 = vektor1[2];
/* Computing 2nd power */
    d__3 = vektor1[3];
    v1 = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
/* Computing 2nd power */
    d__1 = vektor2[1];
/* Computing 2nd power */
    d__2 = vektor2[2];
/* Computing 2nd power */
    d__3 = vektor2[3];
    v2 = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3);
    ret_val = acos((vektor1[1] * vektor2[1] + vektor1[2] * vektor2[2] + 
	    vektor1[3] * vektor2[3]) / (v1 * v2));
    return ret_val;
} /* winkel_ */


/* Subroutine */ int punkt_einer_geraden__(p0, a, skalar, punkt)
doublereal *p0, *a, *skalar, *punkt;
{

/* --- punkt einer geraden aus aufpunkt, richtungsvektor und skalar */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --punkt;
    --a;
    --p0;

    /* Function Body */
    punkt[1] = p0[1] + a[1] * *skalar;
    punkt[2] = p0[2] + a[2] * *skalar;
    punkt[3] = p0[3] + a[3] * *skalar;
    return 0;
} /* punkt_einer_geraden__ */


integer flaechen_schnitt__(type, strahl_p0__, strahl_a__, s_x__, s_v__, s_u__,
	 s_n__, reflex_p0__, reflex_a__, angle)
integer *type;
doublereal *strahl_p0__, *strahl_a__, *s_x__, *s_v__, *s_u__, *s_n__, *
	reflex_p0__, *reflex_a__, *angle;
{
    /* System generated locals */
    integer ret_val;
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int vektor_addition__(), punkt_einer_geraden__();
    static doublereal help[3];
    static integer i, j;
    static doublereal k, m, n, s;
    extern /* Subroutine */ int vektor_mit_skalar__();
    extern doublereal winkel_();
    static doublereal verbindung[3], projektion;
    extern doublereal skalar_produkt__();
    extern /* Subroutine */ int vektor_subtraktion__();


/* ---   prueft ob es einen schnittpunkt des aktuellen strahls mit */
/*       der angew„hlten fl„che gibt, berechnet den schnittpunkt */
/*      mit der fl„chen und generiert den neuen strahl, der von dieser fl„
che*/
/*       ausgeht */


/* --- parameter datei fuer ray */

/*       function */
    /* Parameter adjustments */
    --reflex_a__;
    --reflex_p0__;
    --s_n__;
    --s_u__;
    --s_v__;
    --s_x__;
    --strahl_a__;
    --strahl_p0__;

    /* Function Body */
    ret_val = -1;
    projektion = skalar_produkt__(&strahl_a__[1], &s_n__[1]);
    if (abs(projektion) >= 1e-8) {
	ret_val = 1;
    } else {
/* es gibt keinen */
	ret_val = 0;
	return ret_val;
    }
    vektor_subtraktion__(&strahl_p0__[1], &s_x__[1], verbindung);
    s = -skalar_produkt__(verbindung, &s_n__[1]) / projektion;
    punkt_einer_geraden__(&strahl_p0__[1], &strahl_a__[1], &s, &reflex_p0__[1]
	    );
    ret_val = 0;
    for (i = 1; i <= 3; ++i) {
	for (j = 3; j >= 1; --j) {
	    if (i != j && s_u__[j] != 0.) {
		n = s_v__[i] * s_u__[j] - s_v__[j] * s_u__[i];
		if (abs(n) >= 1e-9) {
		    m = ((reflex_p0__[j] - s_x__[j]) * s_v__[i] - (
			    reflex_p0__[i] - s_x__[i]) * s_v__[j]) / n;
		    if (m > 0. && m < 1.) {
			k = (reflex_p0__[i] - s_x__[i] - m * s_u__[i]) / 
				s_v__[i];
			if (k > 0. && k < 1.) {
			    ret_val = 1;
			    goto L100;
			}
		    }
		}
	    }
	}
    }
L100:
    if (ret_val == 0) {
	return ret_val;
    }

/* winkel zwischen einfallendem strahl und spiegelnder ebene */
/* reflektierter strahl */

    k = skalar_produkt__(&s_n__[1], &strahl_a__[1]) / skalar_produkt__(&s_n__[
	    1], &s_n__[1]);
    d__1 = k * -2.;
    vektor_mit_skalar__(&d__1, &s_n__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, &reflex_a__[1]);

    d__1 = -k;
    vektor_mit_skalar__(&d__1, &s_n__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, help);
    *angle = winkel_(&strahl_a__[1], help);

    if (*type == 1) {
	for (i = 1; i <= 3; ++i) {
	    reflex_a__[i] = strahl_a__[i];
	}
    }
    return ret_val;
} /* flaechen_schnitt__ */



integer flaechen_lb__(strahl_p0__, strahl_a__, s_x__, s_v__, s_u__, s_n__, 
	s_n1__, reflex_p0__, reflex_a__, angle)
doublereal *strahl_p0__, *strahl_a__, *s_x__, *s_v__, *s_u__, *s_n__, *s_n1__,
	 *reflex_p0__, *reflex_a__, *angle;
{
    /* System generated locals */
    integer ret_val;
    doublereal d__1;

    /* Local variables */
    extern /* Subroutine */ int vektor_addition__(), punkt_einer_geraden__();
    static doublereal help[3];
    static integer i, j;
    static doublereal k, m, n, s;
    extern /* Subroutine */ int vektor_mit_skalar__();
    extern doublereal winkel_();
    static doublereal verbindung[3], projektion;
    extern doublereal skalar_produkt__();
    extern /* Subroutine */ int vektor_subtraktion__();


/* ---   prueft ob es einen schnittpunkt des aktuellen strahls mit */
/*       der angewaehlten flaeche gibt, berechnet den schnittpunkt */
/*      mit der flaechen und generiert den neuen strahl, der von dieser fl
aeche*/
/*       ausgeht */


/* --- parameter datei fuer ray */

/*       function */
/* --- init */
    /* Parameter adjustments */
    --reflex_a__;
    --reflex_p0__;
    --s_n1__;
    --s_n__;
    --s_u__;
    --s_v__;
    --s_x__;
    --strahl_a__;
    --strahl_p0__;

    /* Function Body */
    ret_val = -1;

    projektion = skalar_produkt__(&strahl_a__[1], &s_n__[1]);

    if (abs(projektion) >= 1e-8) {
	ret_val = 1;
    } else {
/* es gibt keinen */
	ret_val = 0;
	return ret_val;
    }

    vektor_subtraktion__(&strahl_p0__[1], &s_x__[1], verbindung);

    s = -skalar_produkt__(verbindung, &s_n__[1]) / projektion;

    punkt_einer_geraden__(&strahl_p0__[1], &strahl_a__[1], &s, &reflex_p0__[1]
	    );

    ret_val = 0;
    for (i = 1; i <= 3; ++i) {
	for (j = 3; j >= 1; --j) {
	    if (i != j && s_u__[j] != 0.) {
		n = s_v__[i] * s_u__[j] - s_v__[j] * s_u__[i];
		if (abs(n) >= 1e-9) {
		    m = ((reflex_p0__[j] - s_x__[j]) * s_v__[i] - (
			    reflex_p0__[i] - s_x__[i]) * s_v__[j]) / n;
		    if (m > 0. && m < 1.) {
			k = (reflex_p0__[i] - s_x__[i] - m * s_u__[i]) / 
				s_v__[i];
			if (k > 0. && k < 1.) {
			    ret_val = 1;
			    goto L100;
			}
		    }
		}
	    }
	}
    }
L100:
    if (ret_val == 0) {
	return ret_val;
    }

/* winkel zwischen einfallendem strahl und spiegelnder ebene */
/*             ---> reflektierter strahl */

    k = skalar_produkt__(&s_n1__[1], &strahl_a__[1]) / skalar_produkt__(&
	    s_n1__[1], &s_n1__[1]);
    d__1 = k * -2.;
    vektor_mit_skalar__(&d__1, &s_n1__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, &reflex_a__[1]);

    d__1 = -k;
    vektor_mit_skalar__(&d__1, &s_n1__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, help);
    *angle = winkel_(&strahl_a__[1], help);

    return ret_val;
} /* flaechen_lb__ */

