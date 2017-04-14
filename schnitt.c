/* schnitt.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"


integer flaechen_laue__(strahl_p0__, strahl_a__, s_x__, s_v__, s_u__, 
	s_surf__, s_net__, reflex_p0__, reflex_a__, angle)
doublereal *strahl_p0__, *strahl_a__, *s_x__, *s_v__, *s_u__, *s_surf__, *
	s_net__, *reflex_p0__, *reflex_a__, *angle;
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
    --s_net__;
    --s_surf__;
    --s_u__;
    --s_v__;
    --s_x__;
    --strahl_a__;
    --strahl_p0__;

    /* Function Body */
    ret_val = -1;
    projektion = skalar_produkt__(&strahl_a__[1], &s_surf__[1]);
    if (abs(projektion) >= 1e-8) {
	ret_val = 1;
    } else {
/* es gibt keinen */
	ret_val = 0;
	return ret_val;
    }
    vektor_subtraktion__(&strahl_p0__[1], &s_x__[1], verbindung);
/* pukt auf der geometrischen oberflaeche */
    s = -skalar_produkt__(verbindung, &s_surf__[1]) / projektion;
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

    k = skalar_produkt__(&s_net__[1], &strahl_a__[1]) / skalar_produkt__(&
	    s_net__[1], &s_net__[1]);
    d__1 = k * -2.;
    vektor_mit_skalar__(&d__1, &s_net__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, &reflex_a__[1]);

    d__1 = -k;
    vektor_mit_skalar__(&d__1, &s_net__[1], verbindung);
    vektor_addition__(&strahl_a__[1], verbindung, help);
    *angle = winkel_(&strahl_a__[1], help);

    return ret_val;
} /* flaechen_laue__ */

