/* dev_test.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublereal c_b4 = -.01;
static doublereal c_b5 = .01;
static integer c__50 = 50;
static integer c__2 = 2;
static doublereal c_b8 = -.001;
static doublereal c_b9 = .001;
static integer c__5 = 5;

/* Main program */ MAIN__()
{
    /* System generated locals */
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(), s_wsle(), do_lio(), e_wsle(), f_clos();

    /* Local variables */
    static integer lambda_x__;
    static doublereal strahl_a__[3];
    extern /* Subroutine */ int histinit_();
    static integer i;
    static doublereal strahl_p0__[3];
    static integer l;
    extern /* Subroutine */ int histogram_(), strahl_generieren__(), 
	    histowrite_();

    /* Fortran I/O blocks */
    static cilist io___5 = { 0, 18, 0, 0, 0 };
    static cilist io___7 = { 0, 19, 0, 0, 0 };
    static cilist io___8 = { 0, 6, 0, 0, 0 };



/* --- 	testen der Verteilungsfunktionen */

    o__1.oerr = 0;
    o__1.ounit = 18;
    o__1.ofnmlen = 41;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hit.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 19;
    o__1.ofnmlen = 42;
    o__1.ofnm = "/home/thomas_old/fortran/raytrace/hit1.dat";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    histinit_(&c__1, &c_b4, &c_b5, &c__50);
    histinit_(&c__2, &c_b8, &c_b9, &c__50);
    for (i = 1; i <= 25000; ++i) {
	strahl_generieren__(strahl_p0__, strahl_a__, &lambda_x__);
	s_wsle(&io___5);
	for (l = 1; l <= 3; ++l) {
	    do_lio(&c__5, &c__1, (char *)&strahl_p0__[l - 1], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsle();
	s_wsle(&io___7);
	for (l = 1; l <= 3; ++l) {
	    do_lio(&c__5, &c__1, (char *)&strahl_a__[l - 1], (ftnlen)sizeof(
		    doublereal));
	}
	e_wsle();
	histogram_(&c__1, &strahl_a__[1]);
	histogram_(&c__2, &strahl_a__[2]);
	s_wsle(&io___8);
	do_lio(&c__5, &c__1, (char *)&strahl_a__[1], (ftnlen)sizeof(
		doublereal));
	do_lio(&c__5, &c__1, (char *)&strahl_a__[2], (ftnlen)sizeof(
		doublereal));
	e_wsle();
    }
    cl__1.cerr = 0;
    cl__1.cunit = 18;
    cl__1.csta = 0;
    f_clos(&cl__1);
    cl__1.cerr = 0;
    cl__1.cunit = 19;
    cl__1.csta = 0;
    f_clos(&cl__1);
    histowrite_(&c__1, "h_ver.dat", 9L);
    histowrite_(&c__2, "v_ver.dat", 9L);

} /* MAIN__ */

/* Main program alias */ int dev_test__ () { MAIN__ (); }
