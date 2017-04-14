/* histlib.f -- translated by f2c (version 19940714).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal pbav[10], pbev[10], pblv[10];
    integer iarray[10000]	/* was [10][1000] */, ianz, ndata;
} hist_;

#define hist_1 hist_

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__5 = 5;


/* Subroutine */ int histinit_(inr, xa, xe, hdata)
integer *inr;
doublereal *xa, *xe;
integer *hdata;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer i, j;

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };



/* --- Histogramm inr loeschen und vorbereiten */


/*     ndata niemals groesser 1000, sonst */
/* --- nur veraendern, wenn die felder im common mitgeandert werden */


/* --- commonvariablen fuer Histogrammer */

/*   pblv = dble(ndata) 1000 Arrayelemente */
/*   8 Histogramme */

    hist_1.ndata = 1000;
    hist_1.ianz = 10;

    if (*inr < 1 || *inr >= hist_1.ianz) {
	s_wsle(&io___1);
	do_lio(&c__9, &c__1, " Error: Falsches Histogramm ", 28L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }
    hist_1.pbav[*inr - 1] = *xa;
    hist_1.pbev[*inr - 1] = *xe;
    hist_1.pblv[*inr - 1] = (doublereal) (*hdata);
    i__1 = hist_1.ianz;
    for (j = 1; j <= i__1; ++j) {
	i__2 = hist_1.ndata;
	for (i = 1; i <= i__2; ++i) {
	    hist_1.iarray[j + i * 10 - 11] = 0;
	}
    }
    return 0;
} /* histinit_ */


/* Subroutine */ int histogram_(inr, yhist)
integer *inr;
doublereal *yhist;
{
    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer ix;

    /* Fortran I/O blocks */
    static cilist io___4 = { 0, 6, 0, 0, 0 };
    static cilist io___5 = { 0, 6, 0, 0, 0 };



/* --- commonvariablen fuer Histogrammer */

/*   pblv = dble(ndata) 1000 Arrayelemente */
/*   8 Histogramme */

    if (*inr < 1 || *inr >= hist_1.ianz) {
	s_wsle(&io___4);
	do_lio(&c__9, &c__1, " Error: Falsches Histogramm ", 28L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }
    if (hist_1.pbav[*inr - 1] == 0. && hist_1.pbev[*inr - 1] == 0.) {
	s_wsle(&io___5);
	do_lio(&c__9, &c__1, " Error: Histogramm nicht definiert ", 35L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }

/* --- index im histogram berechnen */

    ix = (integer) ((hist_1.pblv[*inr - 1] - 1.) / (hist_1.pbev[*inr - 1] - 
	    hist_1.pbav[*inr - 1]) * (*yhist - hist_1.pbav[*inr - 1]) + 1.);

    if (ix >= 0 && ix <= (integer) hist_1.pblv[*inr - 1]) {
	++hist_1.iarray[*inr + ix * 10 - 11];
    } else {
/* hier muesste ein Zahler ergaenz werden, der oberhalb/unterhalb summ
iert */
/*           write(6,*) ' Inr_Error : ',inr,'  Histo Bereichsfehler ix
= ',ix*/

    }
    return 0;
} /* histogram_ */


/* Subroutine */ int histowrite_(inr, name, name_len)
integer *inr;
char *name;
ftnlen name_len;
{
    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_len(), f_open(), s_wsle(), do_lio(), e_wsle(), f_clos();

    /* Local variables */
    static integer clen;
    static char name1[12];
    static integer i;
    static doublereal xhilf;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___11 = { 0, 6, 0, 0, 0 };
    static cilist io___13 = { 0, 11, 0, 0, 0 };



/* --- commonvariablen fuer Histogrammer */

/*   pblv = dble(ndata) 1000 Arrayelemente */
/*   8 Histogramme */

    clen = i_len(name, name_len);
    i__1 = clen;
    for (i = 1; i <= i__1; ++i) {
	name1[i - 1] = name[i - 1];
    }
    o__1.oerr = 0;
    o__1.ounit = 11;
    o__1.ofnmlen = name_len;
    o__1.ofnm = name;
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    if (*inr < 1 || *inr >= hist_1.ianz) {
	s_wsle(&io___10);
	do_lio(&c__9, &c__1, " Error: Falsches Histogramm ", 28L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }
    if (hist_1.pbav[*inr - 1] == 0. && hist_1.pbev[*inr - 1] == 0.) {
	s_wsle(&io___11);
	do_lio(&c__9, &c__1, " Error: Histogramm nicht definiert ", 35L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return 0;
    }

/* --- index im histogram berechnen */

    i__1 = (integer) hist_1.pblv[*inr - 1];
    for (i = 1; i <= i__1; ++i) {
	xhilf = (doublereal) (i - 1) * (hist_1.pbev[*inr - 1] - hist_1.pbav[*
		inr - 1]) / (hist_1.pblv[*inr - 1] - 1) + hist_1.pbav[*inr - 
		1];
	s_wsle(&io___13);
	do_lio(&c__5, &c__1, (char *)&xhilf, (ftnlen)sizeof(doublereal));
	do_lio(&c__3, &c__1, (char *)&hist_1.iarray[*inr + i * 10 - 11], (
		ftnlen)sizeof(integer));
	e_wsle();
    }
    cl__1.cerr = 0;
    cl__1.cunit = 11;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* histowrite_ */

integer histoindex_(inr, yhist)
integer *inr;
doublereal *yhist;
{
    /* System generated locals */
    integer ret_val;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle();

    /* Local variables */
    static integer ix;

    /* Fortran I/O blocks */
    static cilist io___14 = { 0, 6, 0, 0, 0 };
    static cilist io___15 = { 0, 6, 0, 0, 0 };



/* --- commonvariablen fuer Histogrammer */

/*   pblv = dble(ndata) 1000 Arrayelemente */
/*   8 Histogramme */

    if (*inr < 1 || *inr >= hist_1.ianz) {
	s_wsle(&io___14);
	do_lio(&c__9, &c__1, " Error: Falsches Histogramm ", 28L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return ret_val;
    }
    if (hist_1.pbav[*inr - 1] == 0. && hist_1.pbev[*inr - 1] == 0.) {
	s_wsle(&io___15);
	do_lio(&c__9, &c__1, " Error: Histogramm nicht definiert ", 35L);
	do_lio(&c__3, &c__1, (char *)&(*inr), (ftnlen)sizeof(integer));
	e_wsle();
	return ret_val;
    }

/* --- index im histogram berechnen */

    ix = (integer) ((hist_1.pblv[*inr - 1] - 1.) / (hist_1.pbev[*inr - 1] - 
	    hist_1.pbav[*inr - 1]) * (*yhist - hist_1.pbav[*inr - 1]) + 1.);

    if (ix >= 0 && ix <= (integer) hist_1.pblv[*inr - 1]) {
	ret_val = ix;
    } else {
	ret_val = -1;
    }
    return ret_val;
} /* histoindex_ */

