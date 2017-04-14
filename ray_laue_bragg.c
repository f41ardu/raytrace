/* ray_laue_bragg.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b16 = 7426.2278322679194;
static integer c__1 = 1;
static doublereal c_b18 = 0.;
static doublereal c_b19 = 1.;
static integer c__5 = 5;
static doublereal c_b223 = -.02;
static doublereal c_b224 = .02;
static integer c__50 = 50;
static integer c__2 = 2;
static doublereal c_b227 = -.002;
static doublereal c_b228 = .002;
static integer c__3 = 3;
static integer c__9 = 9;

/* Main program */ MAIN__()
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open();
    double atan();
    integer s_wsle(), do_lio(), e_wsle(), f_clos();

    /* Local variables */
    extern /* Subroutine */ int dreh_();
    static doublereal lambda_x__, sum_intensitaet__, mnet[9]	/* was [3][3] 
	    */, tnet, strahl_a__[3], b1_px__[3], b2_px__[3], b3_px__[3], 
	    b4_px__[3];
    extern integer flaechen_schnitt__();
    extern /* Subroutine */ int histinit_();
    static doublereal s1_px__[3], s2_px__[3];
    extern /* Subroutine */ int vektor_kreuzprodukt__();
    static integer i;
    static doublereal strahl_p0__[3], m[9]	/* was [3][3] */, bragg, t;
    static integer ilaue;
    extern integer flaechen_laue__();
    extern /* Subroutine */ int trans_();
    static doublereal msurf[9]	/* was [3][3] */, tsurf, neu0_a__[3], 
	    neu1_a__[3], neu2_a__[3], neu3_a__[3], neu4_a__[3], neu5_a__[3];
    extern /* Subroutine */ int histogram_();
    static doublereal s1_net__[3];
    extern /* Subroutine */ int strahl_generieren__();
    static doublereal lambda, dp, pi;
    static integer ibragg;
    static doublereal xd[3], neu0_p0__[3], neu1_p0__[3], neu2_p0__[3], 
	    neu3_p0__[3], neu4_p0__[3], neu5_p0__[3];
    static integer hit_it__;
    static doublereal winkel;
    extern /* Subroutine */ int matrix_();
    static doublereal s1_surf__[3], dpp, dtp, ptd, b1_n__[3], b2_n__[3], 
	    b3_n__[3], b4_n__[3];
    extern /* Subroutine */ int fspline_();
    static doublereal b1_u__[3], b1_v__[3], b2_v__[3], b2_u__[3], b3_v__[3], 
	    b3_u__[3], b4_v__[3], b4_u__[3], s1_n__[3], s2_n__[3], s1_u__[3], 
	    s1_v__[3], s2_v__[3], s2_u__[3];
    static integer number_hits__;

    /* Fortran I/O blocks */
    static cilist io___13 = { 0, 14, 0, 0, 0 };
    static cilist io___14 = { 0, 14, 0, 0, 0 };
    static cilist io___15 = { 0, 14, 0, 0, 0 };
    static cilist io___16 = { 0, 14, 0, 0, 0 };
    static cilist io___17 = { 0, 14, 0, 0, 0 };
    static cilist io___22 = { 0, 15, 0, 0, 0 };
    static cilist io___23 = { 0, 15, 0, 0, 0 };
    static cilist io___24 = { 0, 15, 0, 0, 0 };
    static cilist io___25 = { 0, 15, 0, 0, 0 };
    static cilist io___26 = { 0, 15, 0, 0, 0 };
    static cilist io___31 = { 0, 16, 0, 0, 0 };
    static cilist io___32 = { 0, 16, 0, 0, 0 };
    static cilist io___33 = { 0, 16, 0, 0, 0 };
    static cilist io___34 = { 0, 16, 0, 0, 0 };
    static cilist io___35 = { 0, 16, 0, 0, 0 };
    static cilist io___40 = { 0, 17, 0, 0, 0 };
    static cilist io___41 = { 0, 17, 0, 0, 0 };
    static cilist io___42 = { 0, 17, 0, 0, 0 };
    static cilist io___43 = { 0, 17, 0, 0, 0 };
    static cilist io___44 = { 0, 17, 0, 0, 0 };
    static cilist io___54 = { 0, 12, 0, 0, 0 };
    static cilist io___55 = { 0, 12, 0, 0, 0 };
    static cilist io___56 = { 0, 12, 0, 0, 0 };
    static cilist io___57 = { 0, 12, 0, 0, 0 };
    static cilist io___58 = { 0, 12, 0, 0, 0 };
    static cilist io___63 = { 0, 13, 0, 0, 0 };
    static cilist io___64 = { 0, 13, 0, 0, 0 };
    static cilist io___65 = { 0, 13, 0, 0, 0 };
    static cilist io___66 = { 0, 13, 0, 0, 0 };
    static cilist io___67 = { 0, 13, 0, 0, 0 };
    static cilist io___83 = { 0, 21, 0, 0, 0 };
    static cilist io___86 = { 0, 22, 0, 0, 0 };
    static cilist io___89 = { 0, 23, 0, 0, 0 };
    static cilist io___92 = { 0, 6, 0, 0, 0 };
    static cilist io___93 = { 0, 24, 0, 0, 0 };
    static cilist io___96 = { 0, 6, 0, 0, 0 };
    static cilist io___97 = { 0, 25, 0, 0, 0 };
    static cilist io___100 = { 0, 26, 0, 0, 0 };



/* raytracing fuer Laue Bragg Monochromator */




/* --- parameter datei fuer ray */

/* strahlen */
/* blenden */
/* spiegel */
/* hilfsvariablen */
/* function */

    o__1.oerr = 0;
    o__1.ounit = 12;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/m1.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 13;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/m2.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 14;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/b1.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 15;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/b2.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 16;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/b3.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 17;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/b4.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);

    o__1.oerr = 0;
    o__1.ounit = 21;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb1.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 22;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb2.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 23;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb3.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 24;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb4.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 25;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb5.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 26;
    o__1.ofnmlen = 43;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hitb6.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);

    o__1.oerr = 0;
    o__1.ounit = 30;
    o__1.ofnmlen = 40;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/rc.dat";
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

/* --- initialisierung fuer den reflex */

    ilaue = 1;
    ibragg = 2;
    fspline_(&c_b16, &c__1);

/* --- 1. Blende */

    xd[0] = 8900.;
    xd[1] = 0.;
    xd[2] = 0.;

/* --- */

    b1_px__[0] = -38.5;
    b1_px__[1] = -17.;
    b1_px__[2] = 0.;

    b1_v__[0] = 77.;
    b1_v__[1] = 0.;
    b1_v__[2] = 0.;

    b1_u__[0] = 0.;
    b1_u__[1] = 34.;
    b1_u__[2] = 0.;

    t = dtp * 90.;
    matrix_(&c_b18, &c_b19, &c_b18, &t, m);

/* --- drehen der Blende */

    vektor_kreuzprodukt__(b1_u__, b1_v__, b1_n__);
    dreh_(m, b1_px__, b1_px__);
    dreh_(m, b1_v__, b1_v__);
    dreh_(m, b1_u__, b1_u__);
    dreh_(m, b1_n__, b1_n__);

/* --- tanslation des Aufpunkts */

    trans_(xd, b1_px__, b1_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___13);
    do_lio(&c__5, &c__1, (char *)&b1_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b1_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b1_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___14);
    d__1 = b1_px__[0] + b1_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b1_px__[1] + b1_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b1_px__[2] + b1_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___15);
    d__1 = b1_px__[0] + b1_v__[0] + b1_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b1_px__[1] + b1_v__[1] + b1_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b1_px__[2] + b1_v__[2] + b1_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___16);
    d__1 = b1_px__[0] + b1_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b1_px__[1] + b1_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b1_px__[2] + b1_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___17);
    do_lio(&c__5, &c__1, (char *)&b1_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b1_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b1_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
    cl__1.cerr = 0;
    cl__1.cunit = 14;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* --- 2. Blende */

    xd[0] = 18208.;
    xd[1] = 0.;
    xd[2] = 0.;

/* --- */

    b2_px__[0] = -7.5;
    b2_px__[1] = -12.;
    b2_px__[2] = 0.;

    b2_v__[0] = 15.;
    b2_v__[1] = 0.;
    b2_v__[2] = 0.;

    b2_u__[0] = 0.;
    b2_u__[1] = 24.;
    b2_u__[2] = 0.;

    t = dtp * 90.;
    matrix_(&c_b18, &c_b19, &c_b18, &t, m);

/* --- drehen der Blende */

    vektor_kreuzprodukt__(b2_u__, b2_v__, b2_n__);
    dreh_(m, b2_px__, b2_px__);
    dreh_(m, b2_v__, b2_v__);
    dreh_(m, b2_u__, b2_u__);
    dreh_(m, b2_n__, b2_n__);

/* --- tanslation des Aufpunkts */

    trans_(xd, b2_px__, b2_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___22);
    do_lio(&c__5, &c__1, (char *)&b2_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b2_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b2_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___23);
    d__1 = b2_px__[0] + b2_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b2_px__[1] + b2_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b2_px__[2] + b2_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___24);
    d__1 = b2_px__[0] + b2_v__[0] + b2_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b2_px__[1] + b2_v__[1] + b2_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b2_px__[2] + b2_v__[2] + b2_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___25);
    d__1 = b2_px__[0] + b2_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b2_px__[1] + b2_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b2_px__[2] + b2_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___26);
    do_lio(&c__5, &c__1, (char *)&b2_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b2_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b2_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
    cl__1.cerr = 0;
    cl__1.cunit = 15;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* --- 3. Blende */

    xd[0] = 29078.;
    xd[1] = 0.;
    xd[2] = 0.;

/* --- */

    b3_px__[0] = -1.;
    b3_px__[1] = -5.;
    b3_px__[2] = 0.;

    b3_v__[0] = 2.;
    b3_v__[1] = 0.;
    b3_v__[2] = 0.;

    b3_u__[0] = 0.;
    b3_u__[1] = 10.;
    b3_u__[2] = 0.;

    t = dtp * 90.;
    matrix_(&c_b18, &c_b19, &c_b18, &t, m);

/* --- drehen der Blende */

    vektor_kreuzprodukt__(b3_u__, b3_v__, b3_n__);
    dreh_(m, b3_px__, b3_px__);
    dreh_(m, b3_v__, b3_v__);
    dreh_(m, b3_u__, b3_u__);
    dreh_(m, b3_n__, b3_n__);

/* --- tanslation des Aufpunkts */

    trans_(xd, b3_px__, b3_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___31);
    do_lio(&c__5, &c__1, (char *)&b3_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b3_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b3_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___32);
    d__1 = b3_px__[0] + b3_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b3_px__[1] + b3_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b3_px__[2] + b3_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___33);
    d__1 = b3_px__[0] + b3_v__[0] + b3_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b3_px__[1] + b3_v__[1] + b3_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b3_px__[2] + b3_v__[2] + b3_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___34);
    d__1 = b3_px__[0] + b3_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b3_px__[1] + b3_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b3_px__[2] + b3_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___35);
    do_lio(&c__5, &c__1, (char *)&b3_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b3_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b3_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
    cl__1.cerr = 0;
    cl__1.cunit = 16;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* --- 4. Blende */

    xd[0] = 29458.;
    xd[1] = 0.;
    xd[2] = 20.;

/* --- */

    b4_px__[0] = -1.;
    b4_px__[1] = -5.;
    b4_px__[2] = 0.;

    b4_v__[0] = 2.;
    b4_v__[1] = 0.;
    b4_v__[2] = 0.;

    b4_u__[0] = 0.;
    b4_u__[1] = 10.;
    b4_u__[2] = 0.;

    t = dtp * 90.;
    matrix_(&c_b18, &c_b19, &c_b18, &t, m);

/* --- drehen der Blende */

    vektor_kreuzprodukt__(b4_u__, b4_v__, b4_n__);
    dreh_(m, b4_px__, b4_px__);
    dreh_(m, b4_v__, b4_v__);
    dreh_(m, b4_u__, b4_u__);
    dreh_(m, b4_n__, b4_n__);

/* --- tanslation des Aufpunkts */

    trans_(xd, b4_px__, b4_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___40);
    do_lio(&c__5, &c__1, (char *)&b4_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b4_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b4_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___41);
    d__1 = b4_px__[0] + b4_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b4_px__[1] + b4_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b4_px__[2] + b4_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___42);
    d__1 = b4_px__[0] + b4_v__[0] + b4_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b4_px__[1] + b4_v__[1] + b4_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b4_px__[2] + b4_v__[2] + b4_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___43);
    d__1 = b4_px__[0] + b4_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = b4_px__[1] + b4_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = b4_px__[2] + b4_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___44);
    do_lio(&c__5, &c__1, (char *)&b4_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b4_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&b4_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
    cl__1.cerr = 0;
    cl__1.cunit = 17;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* --- Kristalle */

    bragg = 25.6633364;
/* Si220 */
    tsurf = (bragg + 90.) * dtp;
    tnet = -bragg * dtp;
    matrix_(&c_b18, &c_b19, &c_b18, &tsurf, msurf);
    matrix_(&c_b18, &c_b19, &c_b18, &tnet, mnet);

/* --- translation */

    xd[0] = 29278.;
    xd[1] = 0.;
    xd[2] = 10.;

/* --- */

    s1_px__[0] = -20.;
    s1_px__[1] = -5.;
    s1_px__[2] = 0.;

    s1_v__[0] = 20.;
    s1_v__[1] = 0.;
    s1_v__[2] = 0.;

    s1_u__[0] = 0.;
    s1_u__[1] = 10.;
    s1_u__[2] = 0.;

/* --- drehen */

    vektor_kreuzprodukt__(s1_u__, s1_v__, s1_n__);

    dreh_(msurf, s1_px__, s1_px__);
    dreh_(msurf, s1_v__, s1_v__);
    dreh_(msurf, s1_u__, s1_u__);
    dreh_(msurf, s1_n__, s1_n__);

/* --- translation */

    trans_(xd, s1_px__, s1_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___54);
    do_lio(&c__5, &c__1, (char *)&s1_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s1_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s1_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___55);
    d__1 = s1_px__[0] + s1_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s1_px__[1] + s1_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s1_px__[2] + s1_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___56);
    d__1 = s1_px__[0] + s1_v__[0] + s1_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s1_px__[1] + s1_v__[1] + s1_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s1_px__[2] + s1_v__[2] + s1_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___57);
    d__1 = s1_px__[0] + s1_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s1_px__[1] + s1_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s1_px__[2] + s1_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___58);
    do_lio(&c__5, &c__1, (char *)&s1_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s1_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s1_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
/*        write(12,*) s1_px(1)+s1_n(1),s1_px(2)+s1_n(2),s1_px(3)+s1_n(3) 
*/
    cl__1.cerr = 0;
    cl__1.cunit = 12;
    cl__1.csta = 0;
    f_clos(&cl__1);
/* ---- schnipp */
/* 	bragg = 25.6633364 !Si220 */
    tsurf = (bragg - 180.) * dtp;
    tnet = (bragg - 180.) * dtp;
    matrix_(&c_b18, &c_b19, &c_b18, &tsurf, msurf);
    matrix_(&c_b18, &c_b19, &c_b18, &tnet, mnet);

/* --- translation */

    xd[0] = 29278.;
    xd[1] = 0.;
    xd[2] = 10.;

/* --- */

    s2_px__[0] = -50.;
    s2_px__[1] = -5.;
    s2_px__[2] = 0.;

    s2_v__[0] = 50.;
    s2_v__[1] = 0.;
    s2_v__[2] = 0.;

    s2_u__[0] = 0.;
    s2_u__[1] = 10.;
    s2_u__[2] = 0.;

/* --- drehen */

    vektor_kreuzprodukt__(s2_u__, s2_v__, s2_n__);

    dreh_(msurf, s2_px__, s2_px__);
    dreh_(msurf, s2_v__, s2_v__);
    dreh_(msurf, s2_u__, s2_u__);
    dreh_(mnet, s2_n__, s2_n__);

/* --- translation */

    trans_(xd, s2_px__, s2_px__);

/* --- Kontrollausgabe (3 - D Gnuplot) */

    s_wsle(&io___63);
    do_lio(&c__5, &c__1, (char *)&s2_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s2_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s2_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___64);
    d__1 = s2_px__[0] + s2_v__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s2_px__[1] + s2_v__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s2_px__[2] + s2_v__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___65);
    d__1 = s2_px__[0] + s2_v__[0] + s2_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s2_px__[1] + s2_v__[1] + s2_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s2_px__[2] + s2_v__[2] + s2_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();
    s_wsle(&io___66);
    d__1 = s2_px__[0] + s2_u__[0];
    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
    d__2 = s2_px__[1] + s2_u__[1];
    do_lio(&c__5, &c__1, (char *)&d__2, (ftnlen)sizeof(doublereal));
    d__3 = s2_px__[2] + s2_u__[2];
    do_lio(&c__5, &c__1, (char *)&d__3, (ftnlen)sizeof(doublereal));
    e_wsle();

    s_wsle(&io___67);
    do_lio(&c__5, &c__1, (char *)&s2_px__[0], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s2_px__[1], (ftnlen)sizeof(doublereal));
    do_lio(&c__5, &c__1, (char *)&s2_px__[2], (ftnlen)sizeof(doublereal));
    e_wsle();
    cl__1.cerr = 0;
    cl__1.cunit = 13;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* --- nun gehts los */

    histinit_(&c__1, &c_b223, &c_b224, &c__50);
    histinit_(&c__2, &c_b227, &c_b228, &c__50);

    for (dp = -10.; dp <= 15.; dp += 1.) {

/* Rockingwinkel */
	dpp = dp / 3600.;
	sum_intensitaet__ = 0.;
	number_hits__ = 0;
	tsurf = (bragg + 90. + dpp) * dtp;
	matrix_(&c_b18, &c_b19, &c_b18, &tsurf, msurf);
	tnet = (bragg + dpp) * dtp;
	matrix_(&c_b18, &c_b19, &c_b18, &tnet, mnet);

/* --- translation */

	xd[0] = 29278.;
	xd[1] = 0.;
	xd[2] = 10.;

/* --- */

	s1_px__[0] = -20.;
	s1_px__[1] = -5.;
	s1_px__[2] = 0.;

	s1_v__[0] = 20.;
	s1_v__[1] = 0.;
	s1_v__[2] = 0.;

	s1_u__[0] = 0.;
	s1_u__[1] = 10.;
	s1_u__[2] = 0.;

/* --- drehen */

	vektor_kreuzprodukt__(s1_u__, s1_v__, s1_n__);

	dreh_(msurf, s1_px__, s1_px__);
	dreh_(msurf, s1_v__, s1_v__);
	dreh_(msurf, s1_u__, s1_u__);
	dreh_(msurf, s1_n__, s1_surf__);
	dreh_(mnet, s1_n__, s1_net__);

/* --- translation */

	trans_(xd, s1_px__, s1_px__);

	for (i = 1; i <= 20000; ++i) {

/* Strahlen */
	    strahl_generieren__(strahl_p0__, strahl_a__, &lambda_x__);
	    lambda = (lambda_x__ - .5) * .001 + 1.66289;

	    histogram_(&c__1, &strahl_a__[1]);
	    histogram_(&c__2, &strahl_a__[2]);
/* --- BLENDE 1 */
	    hit_it__ = flaechen_schnitt__(&c__1, strahl_p0__, strahl_a__, 
		    b1_px__, b1_v__, b1_u__, b1_n__, neu0_p0__, neu0_a__, &
		    winkel);
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___83);
	    do_lio(&c__5, &c__3, (char *)&neu0_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/* --- BLENDE 2 */
	    hit_it__ = flaechen_schnitt__(&c__1, neu0_p0__, neu0_a__, b2_px__,
		     b2_v__, b2_u__, b2_n__, neu1_p0__, neu1_a__, &winkel);
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___86);
	    do_lio(&c__5, &c__3, (char *)&neu1_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/* --- BLENDE 3 */
	    hit_it__ = flaechen_schnitt__(&c__1, neu1_p0__, neu1_a__, b3_px__,
		     b3_v__, b3_u__, b3_n__, neu2_p0__, neu2_a__, &winkel);
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___89);
	    do_lio(&c__5, &c__3, (char *)&neu2_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/* --- 1. Monochromatorspiegel */
	    hit_it__ = flaechen_laue__(neu2_p0__, neu2_a__, s1_px__, s1_v__, 
		    s1_u__, s1_surf__, s1_net__, neu3_p0__, neu3_a__, &winkel)
		    ;
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___92);
	    d__1 = winkel * ptd;
	    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    do_lio(&c__5, &c__3, (char *)&neu3_a__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
	    s_wsle(&io___93);
	    do_lio(&c__5, &c__3, (char *)&neu3_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/* --- 2. Monochromatorspiegel */
	    hit_it__ = flaechen_laue__(neu3_p0__, neu3_a__, s2_px__, s2_v__, 
		    s2_u__, s2_n__, s2_n__, neu4_p0__, neu4_a__, &winkel);
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___96);
	    do_lio(&c__9, &c__1, "2: ", 3L);
	    d__1 = winkel * ptd;
	    do_lio(&c__5, &c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    e_wsle();
	    s_wsle(&io___97);
	    do_lio(&c__5, &c__3, (char *)&neu4_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();
/* --- BLENDE 4 */
	    hit_it__ = flaechen_schnitt__(&c__1, neu4_p0__, neu4_a__, b4_px__,
		     b4_v__, b4_u__, b4_n__, neu5_p0__, neu5_a__, &winkel);
	    if (hit_it__ == 0) {
		goto L100;
	    }
	    s_wsle(&io___100);
	    do_lio(&c__5, &c__3, (char *)&neu5_p0__[0], (ftnlen)sizeof(
		    doublereal));
	    e_wsle();

L100:
	    ;
	}

/* Strahlen_loop */
    }
/* Rockingwinkel */
    cl__1.cerr = 0;
    cl__1.cunit = 21;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 22;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 23;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 24;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 25;
    cl__1.csta = 0;
    f_clos(&cl__1);
} /* MAIN__ */

/* Main program alias */ int raytrace_ () { MAIN__ (); }
