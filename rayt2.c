/* rayt2.f -- translated by f2c (version of 27 June 1992  14:50:07).
   You must link the resulting object file with the libraries:
	-lF77 -lI77 -lm -lc   (in that order)
*/

#include "f2c.h"

doublereal skalar_produkt__(vektor1, vektor2)
doublereal *vektor1, *vektor2;
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static integer iloop;


/* ---   vektor skalarproduk */



/* --- parameter datei fuer ray */

    /* Parameter adjustments */
    --vektor2;
    --vektor1;

    /* Function Body */
    ret_val = 0.;
    for (iloop = 1; iloop <= 3; ++iloop) {
	ret_val = ret_val + vektor1[iloop] + vektor2[iloop];
    }
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

/* ---   vektor addition */



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


doublereal flaechen_schnitt__(strahl_p0__, strahl_a__, s_x__, s_v__, s_u__, 
	s_n__, neu_p0__, neu_a__)
doublereal *strahl_p0__, *strahl_a__, *s_x__, *s_v__, *s_u__, *s_n__, *
	neu_p0__, *neu_a__;
{
    /* System generated locals */
    doublereal ret_val, d__1;

    /* Local variables */
    extern /* Subroutine */ int vektor_addition__(), punkt_einer_geraden__();
    static doublereal s, differenz[3], punkt[3];
    extern /* Subroutine */ int vektor_mit_skalar__();
    static doublereal verbindung[3], projektion;
    extern doublereal skalar_produkt__();
    static integer index_i__;
    extern /* Subroutine */ int vektor_subtraktion__();


/* ---   prft ob es einen schnittpunkt des aktuellen strahls mit */
/*       der angew„hlten fl„che gibt, berechnet den schnittpunkt */
/*      mit der fl„chen und generiert den neuen strahl, der von dieser fl„
che*/
/*       ausgeht */


/* --- parameter datei fuer ray */

/*       function */
    /* Parameter adjustments */
    --neu_a__;
    --neu_p0__;
    --s_n__;
    --s_u__;
    --s_v__;
    --s_x__;
    --strahl_a__;
    --strahl_p0__;

    /* Function Body */
    ret_val = 0.;
    projektion = skalar_produkt__(&strahl_a__[1], &s_n__[1]);
    if (projektion == 0.) {
/* es gibt keinen */
	ret_val = -1.;
	return ret_val;
    }
    vektor_subtraktion__(&strahl_p0__[1], &s_x__[1], verbindung);
    s = -skalar_produkt__(verbindung, &s_n__[1]) / projektion;
    punkt_einer_geraden__(&strahl_p0__[1], &strahl_a__[1], &s, punkt);
    d__1 = projektion * -2.;
    vektor_mit_skalar__(&d__1, &s_n__[1], differenz);
    vektor_addition__(&strahl_a__[1], differenz, &neu_a__[1]);
    for (index_i__ = 1; index_i__ <= 3; ++index_i__) {
	strahl_p0__[index_i__] = punkt[index_i__ - 1];
    }
    return ret_val;
} /* flaechen_schnitt__ */

