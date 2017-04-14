/* ray3.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b5 = 7426.2278322679194;
static integer c__1 = 1;
static doublereal c_b7 = 0.;
static doublereal c_b8 = 1.;
static doublereal c_b20 = 1.66;
static doublereal c_b21 = 1.67;
static integer c__100 = 100;
static integer c__2 = 2;
static integer c__5 = 5;

/* Main program */ MAIN__()
{
    /* Format strings */
    static char fmt_111[] = "(3f12.6,i10)";

    /* System generated locals */
    doublereal d__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open();
    double atan(), tan();
    integer s_wsfe(), do_fio(), e_wsfe(), s_wsle(), do_lio(), e_wsle(), 
	    f_clos();

    /* Local variables */
    extern /* Subroutine */ int dreh_();
    static doublereal lambda_x__, sum_intensitaet__;
    static integer l_ix__, l_ix_det__;
    static doublereal strahl_a__[3], mc_trans__, b1_px__[3], b2_px__[3];
    extern /* Subroutine */ int histinit_();
    extern doublereal flaechen_schnitt__();
    static doublereal s1_px__[3], s2_px__[3];
    extern /* Subroutine */ int vektor_kreuzprodukt__();
    static integer i;
    static doublereal strahl_p0__[3], m[9]	/* was [3][3] */, bragg, t;
    extern /* Subroutine */ int trans_();
    static doublereal neu0_a__[3], neu1_a__[3], neu2_a__[3], neu3_a__[3];
    extern /* Subroutine */ int histogram_(), strahl_generieren__();
    static doublereal lambda, dp, pi, xd[3], neu0_p0__[3], neu1_p0__[3], 
	    neu2_p0__[3], neu3_p0__[3];
    static integer hit_it__;
    extern doublereal reflex_();
    static doublereal winkel;
    extern /* Subroutine */ int matrix_();
    extern integer histoindex_();
    extern /* Subroutine */ int histowrite_();
    static doublereal dpp, dtp, ptd, cut, b1_n__[3], b2_n__[3];
    extern /* Subroutine */ int fspline_();
    static doublereal b1_u__[3], b1_v__[3], b2_v__[3], b2_u__[3], s1_n__[3], 
	    s2_n__[3], s1_u__[3], s1_v__[3], s2_v__[3], s2_u__[3];
    static integer number_hits__;
    static doublereal intensitaet;

    /* Fortran I/O blocks */
    static cilist io___48 = { 0, 30, 0, fmt_111, 0 };
    static cilist io___49 = { 0, 6, 0, 0, 0 };


/*     interferometer */
/*     noch berechnen wir nur den Monochromator */
/*     Idee: ausgehend von einem Punkt S auf einer detektor- oder */
/*           filmebenen wird ein gerader strahl bis in die quelle */
/*           Åber alle spiegel des interferometers zurÅckverfolgt */
/*           und dann der phasenunterschied zwischen den strahlen */
/*           Åber beide wege verglichen */
/*     1. Iteration  geometrische NÑherung */
/*        d.h. keine dynamischen reflexionen keine eindringtiefen */
/*             etc. */

/*        es werden folgende unterprogramme benîtigt */

/*        dreh: fuehrt drehung eines punktes um den koordinatenursprung */
/*              aus */
/*        trans: fuehrt eine translation aus */
/*        flaechen_schnitt: berechnet fuer einen strahl aus der quelle */
/*           den schnittpunkt mit einer blende, oder den reflektierten */
/*           strahl an einem spiegel */

/*        beschreibung der daten */

/*        ein spiegel oder eine blende wird durch folgende */
/*        felder definiert ein aufpunkt und 2 richtungsvektoren */

/*        bi_ : Blenden */
/*        bi_x(3) Aufpunkt der blende i */
/*        bi_u(3) Richtungsvektor 1 des biegers i */
/*        bi_v(3) Richtungsvektor 2 des biegers i */
/*        bi_n(3) Orientierungsvektor der oberflÑche n = u x v */

/*        si_ : Spiegel */
/*        si_x(3) Aufpunkt des spiegels i */
/*        si_u(3) Richtungsvektor 1 des spiegels i */
/*        si_v(3) Richtungsvektor 2 des spiegels i */
/*        si_n(3) Orientierungsvektor der oberflÑche n = u x v */
/*       offset(i) beschreibt den offsetwinkel der netzebene zur oberflÑch
e*/
/*       damit kînnen in der oberflÑche h und l netzebene beschrieben werd
en*/
/*        das entspricht dem asymmetriewinkel der dynamischen theorie */

/*        ein strahl wird beschrieben durch einen */
/*        aufpunkt und durch eine richtung */

/*        s(3) : aufpunkt */
/*        r(3) : richtungsvektor */





/* --- parameter datei fuer ray */

/* strahlen */
/* blenden */
/* spiegel */

/* function */

/*        open(11,'m1.dat') */
/*        open(12,'m2.dat') */
/*        open(15,'b2.dat') */
/*        open(13,'strahl.dat') */
    o__1.oerr = 0;
    o__1.ounit = 30;
    o__1.ofnmlen = 47;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/rock_temp.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 31;
    o__1.ofnmlen = 48;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/rockd_temp.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);

/* L111: */

    pi = atan(1.) * 4.;
    dtp = pi / 180.;
    ptd = 180. / pi;

/* --- init fuer reflex */

    fspline_(&c_b5, &c__1);


/*  translation 1. blende */

    xd[0] = 17500.;
    xd[1] = 0.;
    xd[2] = 0.;

/*  erzeugen der 1.blende */

    b1_px__[0] = -1.;
/* -.5 */
    b1_px__[1] = -18.;
/* -9. */
    b1_px__[2] = 0.;

    b1_v__[0] = 2.;
/* 1. */
    b1_v__[1] = 0.;
    b1_v__[2] = 0.;

    b1_u__[0] = 0.;
    b1_u__[1] = 36.;
/* 18. */
    b1_u__[2] = 0.;
    t = dtp * -90.;
    matrix_(&c_b7, &c_b8, &c_b7, &t, m);

/*  drehen der 1. blende */

    vektor_kreuzprodukt__(b1_u__, b1_v__, b1_n__);
    dreh_(m, b1_px__, b1_px__);
    dreh_(m, b1_v__, b1_v__);
    dreh_(m, b1_u__, b1_u__);
    dreh_(m, b1_n__, b1_n__);

/*  translation des aufpunkts */

    trans_(xd, b1_px__, b1_px__);

/*  translation 2. blende */

    xd[0] = 1.9e4;
    xd[1] = 0.;
    xd[2] = -40.;

/*  erzeugen der 2.blende */

    b2_px__[0] = -1.;
    b2_px__[1] = -9.;
    b2_px__[2] = 0.;

    b2_v__[0] = 2.;
    b2_v__[1] = 0.;
    b2_v__[2] = 0.;

    b2_u__[0] = 0.;
    b2_u__[1] = 18.;
    b2_u__[2] = 0.;
    t = dtp * -90.;
    matrix_(&c_b7, &c_b8, &c_b7, &t, m);

/*  drehen der 2. blende */

    vektor_kreuzprodukt__(b2_u__, b2_v__, b2_n__);
    dreh_(m, b2_px__, b2_px__);
    dreh_(m, b2_v__, b2_v__);
    dreh_(m, b2_u__, b2_u__);
    dreh_(m, b2_n__, b2_n__);

/*  translation des aufpunkts */

    trans_(xd, b2_px__, b2_px__);
/* und Kontrollausgabe fuer einen Gnu 3-D PLot */
/*        write(15,*) b2_px(1),b2_px(2),b2_px(3) */

/*        write(15,*) b2_px(1)+b2_v(1),b2_px(2)+b2_v(2), */
/*     .              b2_px(3)+b2_v(3) */
/*        write(15,*) b2_px(1)+b2_v(1)+b2_u(1),b2_px(2)+b2_v(2)+ */
/*     .              b2_u(2),b2_px(3)+b2_v(3)+b2_u(3) */
/*        write(15,*) b2_px(1)+b2_u(1),b2_px(2)+b2_u(2), */
/*     .              b2_px(3)+b2_u(3) */

/*        write(15,*) b2_px(1),b2_px(2),b2_px(3) */

/* --- Monochromatoroffset cut */

    cut = 40.;

/* drehwinkel + drehmatrix  des ersten Kristalls */

/*        write(6,*) ' Winkel ' */
/*        read(5,*) winkel */
    bragg = 15.379296;
/*       bragg = 15.37731167 !Si 111 bei 1.66289 Angstr. mit Brechungskorr
ektur*/
/* Si 111 bei 1.66289 Angstr. ohne Brechungkorre */
    t = -bragg * dtp;
    matrix_(&c_b7, &c_b8, &c_b7, &t, m);

/*  translation 1. Kristall */

    xd[0] = 1.8e4;
    xd[1] = 0.;
    xd[2] = 0.;

/*  erzeugen des 1. */

    s1_px__[0] = -25.;
    s1_px__[1] = -9.;
    s1_px__[2] = 0.;

    s1_v__[0] = 50.;
    s1_v__[1] = 0.;
    s1_v__[2] = 0.;

    s1_u__[0] = 0.;
    s1_u__[1] = 18.;
    s1_u__[2] = 0.;

/*  drehen des 1. kristall */

    vektor_kreuzprodukt__(s1_u__, s1_v__, s1_n__);
    dreh_(m, s1_px__, s1_px__);
    dreh_(m, s1_v__, s1_v__);
    dreh_(m, s1_u__, s1_u__);
    dreh_(m, s1_n__, s1_n__);

/*  translation des aufpunkts */

    trans_(xd, s1_px__, s1_px__);

/* und Kontrollausgabe fuer einen Gnu 3-D PLot */
/*        write(11,*) s1_px(1),s1_px(2),s1_px(3) */

/*        write(11,*) s1_px(1)+s1_v(1),s1_px(2)+s1_v(2), */
/*     .              s1_px(3)+s1_v(3) */
/*        write(11,*) s1_px(1)+s1_v(1)+s1_u(1),s1_px(2)+s1_v(2)+ */
/*     .              s1_u(2),s1_px(3)+s1_v(3)+s1_u(3) */
/*        write(11,*) s1_px(1)+s1_u(1),s1_px(2)+s1_u(2), */
/*     .              s1_px(3)+s1_u(3) */

/*        write(11,*) s1_px(1),s1_px(2),s1_px(3) */

/*  translation 2. Kristall */

    xd[0] = 1.8e4;
    xd[1] = 0.;
    xd[2] = 0.;

/*  erzeugen des 2. */

    s2_px__[0] = -25.;
    s2_px__[1] = -9.;
    s2_px__[2] = 0.;

    s2_v__[0] = 50.;
    s2_v__[1] = 0.;
    s2_v__[2] = 0.;

    s2_u__[0] = 0.;
    s2_u__[1] = 18.;
    s2_u__[2] = 0.;

/*  drehen des 2. kristalls */
    t = -(bragg + 180.) * dtp;
    matrix_(&c_b7, &c_b8, &c_b7, &t, m);

    vektor_kreuzprodukt__(s2_u__, s2_v__, s2_n__);
    dreh_(m, s2_px__, s2_px__);
    dreh_(m, s2_v__, s2_v__);
    dreh_(m, s2_u__, s2_u__);
    dreh_(m, s2_n__, s2_n__);

/*  translation des aufpunkts */

    trans_(xd, s2_px__, s2_px__);

/*  monochromatortranslation (nur. der 2. MC - Kristall */

    if (t != 0.) {
	mc_trans__ = cut / tan(t * -2.);
    } else {
	mc_trans__ = 0.;
    }
    xd[0] = mc_trans__;
    xd[1] = 0.;
    xd[2] = -cut;
    trans_(xd, s2_px__, s2_px__);

/* und Kontrollausgabe fuer einen Gnu 3-D PLot */

/*        write(12,*) s2_px(1),s2_px(2),s2_px(3) */

/*        write(12,*) s2_px(1)+s2_v(1),s2_px(2)+s2_v(2), */
/*     .              s2_px(3)+s2_v(3) */
/*        write(12,*) s2_px(1)+s2_v(1)+s2_u(1),s2_px(2)+s2_v(2)+ */
/*     .              s2_u(2),s2_px(3)+s2_v(3)+s2_u(3) */
/*        write(12,*) s2_px(1)+s2_u(1),s2_px(2)+s2_u(2), */
/*     .              s2_px(3)+s2_u(3) */

/*        write(12,*) s2_px(1),s2_px(2),s2_px(3) */


/*        close(11) */
/*        close(12) */

/* ab hier kommen die Strahlen */

    histinit_(&c__1, &c_b20, &c_b21, &c__100);
/*        call histinit(2,1.66284,1.66294,10) */
    for (dp = -30.; dp <= 30.; dp += .5) {
	dpp = dp / 3600.;
	sum_intensitaet__ = 0.;
	number_hits__ = 0;

/*  translation 2. Kristall */

	xd[0] = 1.8e4;
	xd[1] = 0.;
	xd[2] = 0.;

/*  erzeugen des 2. */

	s2_px__[0] = -25.;
	s2_px__[1] = -9.;
	s2_px__[2] = 0.;

	s2_v__[0] = 50.;
	s2_v__[1] = 0.;
	s2_v__[2] = 0.;

	s2_u__[0] = 0.;
	s2_u__[1] = 18.;
	s2_u__[2] = 0.;

/*  drehen des 2. kristalls */
	t = -(bragg + dpp + 180.) * dtp;
	matrix_(&c_b7, &c_b8, &c_b7, &t, m);

	vektor_kreuzprodukt__(s2_u__, s2_v__, s2_n__);
	dreh_(m, s2_px__, s2_px__);
	dreh_(m, s2_v__, s2_v__);
	dreh_(m, s2_u__, s2_u__);
	dreh_(m, s2_n__, s2_n__);

/*  translation des aufpunkts */

	trans_(xd, s2_px__, s2_px__);

/*  monochromatortranslation (nur. der 2. MC - Kristall */

	if (t != 0.) {
	    mc_trans__ = cut / tan(t * -2.);
	} else {
	    mc_trans__ = 0.;
	}
	xd[0] = mc_trans__;
	xd[1] = 0.;
	xd[2] = -cut;
	trans_(xd, s2_px__, s2_px__);

	for (i = 1; i <= 4000; ++i) {

	    strahl_generieren__(strahl_p0__, strahl_a__, &lambda_x__);
	    lambda = (lambda_x__ - .5) * .01 + 1.66289;
/* 100.e-5 */
	    l_ix__ = histoindex_(&c__1, &lambda);
	    l_ix_det__ = histoindex_(&c__2, &lambda);
/* blende 1 */
	    hit_it__ = (integer) flaechen_schnitt__(strahl_p0__, strahl_a__, 
		    b1_px__, b1_v__, b1_u__, b1_n__, neu0_p0__, neu0_a__, &
		    winkel);
	    if (hit_it__ == 1) {
	    } else {
		goto L100;
	    }
/* 1. monochromatorspiegel */
	    hit_it__ = (integer) flaechen_schnitt__(neu0_p0__, neu0_a__, 
		    s1_px__, s1_v__, s1_u__, s1_n__, neu1_p0__, neu1_a__, &
		    winkel);
	    if (hit_it__ == 1) {
		winkel *= ptd;
		intensitaet = reflex_(&lambda, &winkel);
		if (intensitaet == 0.) {
		    goto L100;
		}
	    } else {
		goto L100;
	    }
/* 2. monochromatorspiegel */
	    hit_it__ = (integer) flaechen_schnitt__(neu1_p0__, neu1_a__, 
		    s2_px__, s2_v__, s2_u__, s2_n__, neu2_p0__, neu2_a__, &
		    winkel);

	    if (hit_it__ == 1) {
		winkel *= ptd;
		intensitaet *= reflex_(&lambda, &winkel);
		if (intensitaet == 0.) {
		    goto L100;
		}
	    } else {
		goto L100;
	    }
/* blende 2 */
	    hit_it__ = (integer) flaechen_schnitt__(neu2_p0__, neu2_a__, 
		    b2_px__, b2_v__, b2_u__, b2_n__, neu3_p0__, neu3_a__, &
		    winkel);
	    if (hit_it__ == 1) {
		sum_intensitaet__ += intensitaet;
		++number_hits__;
	    } else {
		goto L100;
	    }
	    histogram_(&c__1, &lambda);
/*        Write(6,*) ' Sum: ',Sum_Intensitaet */
L100:

	    ;
	}
	s_wsfe(&io___48);
	do_fio(&c__1, (char *)&dp, (ftnlen)sizeof(doublereal));
	d__1 = bragg + dpp;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sum_intensitaet__, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&number_hits__, (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsle(&io___49);
	do_lio(&c__5, &c__1, (char *)&dp, (ftnlen)sizeof(doublereal));
	e_wsle();
    }
/*        close(13) */
    cl__1.cerr = 0;
    cl__1.cunit = 30;
    cl__1.csta = 0;
    f_clos(&cl__1);
    histowrite_(&c__1, "l_ver.dat", 9L);
} /* MAIN__ */

/* Main program alias */ int raytrace_ () { MAIN__ (); }
