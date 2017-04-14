/* blr8.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static doublereal c_b57 = 1.;

/*  23/11/92 211231328  MEMBER NAME  BLSING   (EISPACK.)    FVS         000000
00*/
integer isamax_(n, sx, incx)
integer *n;
doublereal *sx;
integer *incx;
{
    /* System generated locals */
    integer ret_val, i__1;
    doublereal d__1;

    /* Local variables */
    static doublereal smax;
    static integer i, ix;

/*                                                                      00
000020*/
/*    FINDS THE INDEX OF ELEMENT HAVING MAX. ABSOLUTE VALUE.            00
000030*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000040*/
/*                                                                      00
000050*/
/*                                                                      00
000080*/
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    ret_val = 0;
    if (*n < 1) {
	return ret_val;
    }
    ret_val = 1;
    if (*n == 1) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      00
000140*/
/*       CODE FOR INCREMENT NOT EQUAL TO 1                              00
000150*/
/*                                                                      00
000160*/
    ix = 1;
    smax = abs(sx[1]);
    ix += *incx;
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	if ((d__1 = sx[ix], abs(d__1)) <= smax) {
	    goto L5;
	}
	ret_val = i;
	smax = (d__1 = sx[ix], abs(d__1));
L5:
	ix += *incx;
/* L10: */
    }
    return ret_val;
/*                                                                      00
000270*/
/*       CODE FOR INCREMENT EQUAL TO 1                                  00
000280*/
/*                                                                      00
000290*/
L20:
    smax = abs(sx[1]);
    i__1 = *n;
    for (i = 2; i <= i__1; ++i) {
	if ((d__1 = sx[i], abs(d__1)) <= smax) {
	    goto L30;
	}
	ret_val = i;
	smax = (d__1 = sx[i], abs(d__1));
L30:
	;
    }
    return ret_val;
} /* isamax_ */

doublereal sasum_(n, sx, incx)
integer *n;
doublereal *sx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val, d__1, d__2, d__3, d__4, d__5, d__6;

    /* Local variables */
    static integer i, m, nincx;
    static doublereal stemp;
    static integer mp1;

/*                                                                      00
000020*/
/*    TAKES THE SUM OF THE ABSOLUTE VALUES.                             00
000030*/
/*    USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.                   00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    ret_val = 0.;
    stemp = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      00
000140*/
/*       CODE FOR INCREMENT NOT EQUAL TO 1                              00
000150*/
/*                                                                      00
000160*/
    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	stemp += (d__1 = sx[i], abs(d__1));
/* L10: */
    }
    ret_val = stemp;
    return ret_val;
/*                                                                      00
000230*/
/*       CODE FOR INCREMENT EQUAL TO 1                                  00
000240*/
/*                                                                      00
000250*/
/*                                                                      00
000260*/
/*       CLEAN-UP LOOP                                                  00
000270*/
/*                                                                      00
000280*/
L20:
    m = *n % 6;
    if (m == 0) {
	goto L40;
    }
    i__2 = m;
    for (i = 1; i <= i__2; ++i) {
	stemp += (d__1 = sx[i], abs(d__1));
/* L30: */
    }
    if (*n < 6) {
	goto L60;
    }
L40:
    mp1 = m + 1;
    i__2 = *n;
    for (i = mp1; i <= i__2; i += 6) {
	stemp = stemp + (d__1 = sx[i], abs(d__1)) + (d__2 = sx[i + 1], abs(
		d__2)) + (d__3 = sx[i + 2], abs(d__3)) + (d__4 = sx[i + 3], 
		abs(d__4)) + (d__5 = sx[i + 4], abs(d__5)) + (d__6 = sx[i + 5]
		, abs(d__6));
/* L50: */
    }
L60:
    ret_val = stemp;
    return ret_val;
} /* sasum_ */

/* Subroutine */ int saxpy_(n, sa, sx, incx, sy, incy)
integer *n;
doublereal *sa, *sx;
integer *incx;
doublereal *sy;
integer *incy;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, m, ix, iy, mp1;

/*                                                                      00
000020*/
/*    CONSTANT TIMES A VECTOR PLUS A VECTOR.                            00
000030*/
/*    USES UNROLLED LOOP FOR INCREMENTS EQUAL TO ONE.                   00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*sa == 0.) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      00
000130*/
/*       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00
000140*/
/*         NOT EQUAL TO 1                                               00
000150*/
/*                                                                      00
000160*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	sy[iy] += *sa * sx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      00
000270*/
/*       CODE FOR BOTH INCREMENTS EQUAL TO 1                            00
000280*/
/*                                                                      00
000290*/
/*                                                                      00
000300*/
/*       CLEAN-UP LOOP                                                  00
000310*/
/*                                                                      00
000320*/
L20:
    m = *n % 4;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	sy[i] += *sa * sx[i];
/* L30: */
    }
    if (*n < 4) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= i__1; i += 4) {
	sy[i] += *sa * sx[i];
	sy[i + 1] += *sa * sx[i + 1];
	sy[i + 2] += *sa * sx[i + 2];
	sy[i + 3] += *sa * sx[i + 3];
/* L50: */
    }
    return 0;
} /* saxpy_ */

/* Subroutine */ int scopy_(n, sx, incx, sy, incy)
integer *n;
doublereal *sx;
integer *incx;
doublereal *sy;
integer *incy;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, m, ix, iy, mp1;

/*                                                                      00
000020*/
/*    COPIES A VECTOR, X, TO A VECTOR, Y.                               00
000030*/
/*    USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.                    00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      00
000120*/
/*       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00
000130*/
/*         NOT EQUAL TO 1                                               00
000140*/
/*                                                                      00
000150*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	sy[iy] = sx[ix];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      00
000260*/
/*       CODE FOR BOTH INCREMENTS EQUAL TO 1                            00
000270*/
/*                                                                      00
000280*/
/*                                                                      00
000290*/
/*       CLEAN-UP LOOP                                                  00
000300*/
/*                                                                      00
000310*/
L20:
    m = *n % 7;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	sy[i] = sx[i];
/* L30: */
    }
    if (*n < 7) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= i__1; i += 7) {
	sy[i] = sx[i];
	sy[i + 1] = sx[i + 1];
	sy[i + 2] = sx[i + 2];
	sy[i + 3] = sx[i + 3];
	sy[i + 4] = sx[i + 4];
	sy[i + 5] = sx[i + 5];
	sy[i + 6] = sx[i + 6];
/* L50: */
    }
    return 0;
} /* scopy_ */

doublereal sdot_(n, sx, incx, sy, incy)
integer *n;
doublereal *sx;
integer *incx;
doublereal *sy;
integer *incy;
{
    /* System generated locals */
    integer i__1;
    doublereal ret_val;

    /* Local variables */
    static integer i, m;
    static doublereal stemp;
    static integer ix, iy, mp1;

/*                                                                      00
000020*/
/*    FORMS THE DOT PRODUCT OF TWO VECTORS.                             00
000030*/
/*    USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.                  00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    stemp = 0.;
    ret_val = 0.;
    if (*n <= 0) {
	return ret_val;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      00
000140*/
/*       CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS                00
000150*/
/*         NOT EQUAL TO 1                                               00
000160*/
/*                                                                      00
000170*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	stemp += sx[ix] * sy[iy];
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    ret_val = stemp;
    return ret_val;
/*                                                                      00
000290*/
/*       CODE FOR BOTH INCREMENTS EQUAL TO 1                            00
000300*/
/*                                                                      00
000310*/
/*                                                                      00
000320*/
/*       CLEAN-UP LOOP                                                  00
000330*/
/*                                                                      00
000340*/
L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	stemp += sx[i] * sy[i];
/* L30: */
    }
    if (*n < 5) {
	goto L60;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= i__1; i += 5) {
	stemp = stemp + sx[i] * sy[i] + sx[i + 1] * sy[i + 1] + sx[i + 2] * 
		sy[i + 2] + sx[i + 3] * sy[i + 3] + sx[i + 4] * sy[i + 4];
/* L50: */
    }
L60:
    ret_val = stemp;
    return ret_val;
} /* sdot_ */

doublereal smach_(job)
integer *job;
{
    /* System generated locals */
    doublereal ret_val;

    /* Local variables */
    static doublereal huge, tiny, s, eps;

/*                                                                      00
000030*/
/*    SMACH COMPUTES MACHINE PARAMETERS OF FLOATING POINT               00
000040*/
/*    ARITHMETIC FOR USE IN TESTING ONLY.  NOT REQUIRED BY              00
000050*/
/*    LINPACK PROPER.                                                   00
000060*/
/*                                                                      00
000070*/
/*    IF TROUBLE WITH AUTOMATIC COMPUTATION OF THESE QUANTITIES,        00
000080*/
/*    THEY CAN BE SET BY DIRECT ASSIGNMENT STATEMENTS.                  00
000090*/
/*    ASSUME THE COMPUTER HAS                                           00
000100*/
/*                                                                      00
000110*/
/*       B = BASE OF ARITHMETIC                                         00
000120*/
/*       T = NUMBER OF BASE  B  DIGITS                                  00
000130*/
/*       L = SMALLEST POSSIBLE EXPONENT                                 00
000140*/
/*       U = LARGEST POSSIBLE EXPONENT                                  00
000150*/
/*                                                                      00
000160*/
/*    THEN                                                              00
000170*/
/*                                                                      00
000180*/
/*       EPS = B**(1-T)                                                 00
000190*/
/*       TINY = 100.0*B**(-L+T)                                         00
000200*/
/*       HUGE = 0.01*B**(U-T)                                           00
000210*/
/*                                                                      00
000220*/
/*    DMACH SAME AS SMACH EXCEPT T, L, U APPLY TO                       00
000230*/
/*    DOUBLE PRECISION.                                                 00
000240*/
/*                                                                      00
000250*/
/*    CMACH SAME AS SMACH EXCEPT IF COMPLEX DIVISION                    00
000260*/
/*    IS DONE BY                                                        00
000270*/
/*                                                                      00
000280*/
/*       1/(X+I*Y) = (X-I*Y)/(X**2+Y**2)                                00
000290*/
/*                                                                      00
000300*/
/*    THEN                                                              00
000310*/
/*                                                                      00
000320*/
/*       TINY = SQRT(TINY)                                              00
000330*/
/*       HUGE = SQRT(HUGE)                                              00
000340*/
/*                                                                      00
000350*/
/*                                                                      00
000360*/
/*    JOB IS 1, 2 OR 3 FOR EPSILON, TINY AND HUGE, RESPECTIVELY.        00
000370*/
/*                                                                      00
000380*/
/*                                                                      00
000390*/
/*                                                                      00
000410*/
    eps = 1.;
L10:
    eps /= 2.;
    s = eps + 1.;
    if (s > 1.) {
	goto L10;
    }
    eps *= 2.;
/*                                                                      00
000470*/
    s = 1.;
L20:
    tiny = s;
    s /= 16.;
    if (s * 100. != 0.) {
	goto L20;
    }
    tiny = tiny / eps * 100.;
    huge = 1. / tiny;
/*                                                                      00
000540*/
    if (*job == 1) {
	ret_val = eps;
    }
    if (*job == 2) {
	ret_val = tiny;
    }
    if (*job == 3) {
	ret_val = huge;
    }
    return ret_val;
} /* smach_ */

doublereal snrm2_(n, sx, incx)
integer *n;
doublereal *sx;
integer *incx;
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;
    static doublereal cutlo = 4.441e-16;
    static doublereal cuthi = 1.304e19;

    /* Format strings */
    static char fmt_30[] = "";
    static char fmt_50[] = "";
    static char fmt_70[] = "";
    static char fmt_110[] = "";

    /* System generated locals */
    integer i__1, i__2;
    doublereal ret_val, d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal xmax;
    static integer next, i, j, nn;
    static doublereal hitest, sum;

    /* Assigned format variables */
    char *next_fmt;

    /* Parameter adjustments */
    --sx;

    /* Function Body */
/*                                                                      00
000050*/
/*    EUCLIDEAN NORM OF THE N-VECTOR STORED IN SX() WITH STORAGE        00
000060*/
/*    INCREMENT INCX .                                                  00
000070*/
/*    IF    N .LE. 0 RETURN WITH RESULT = 0.                            00
000080*/
/*    IF N .GE. 1 THEN INCX MUST BE .GE. 1                              00
000090*/
/*                                                                      00
000100*/
/*          C.L.LAWSON, 1978 JAN 08                                     00
000110*/
/*                                                                      00
000120*/
/*    FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE       00
000130*/
/*    HOPEFULLY APPLICABLE TO ALL MACHINES.                             00
000140*/
/*        CUTLO = MAXIMUM OF  SQRT(U/EPS)  OVER ALL KNOWN MACHINES.     00
000150*/
/*        CUTHI = MINIMUM OF  SQRT(V)      OVER ALL KNOWN MACHINES.     00
000160*/
/*    WHERE                                                             00
000170*/
/*        EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.                 00
000180*/
/*        U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)               00
000190*/
/*        V   = LARGEST  NO.            (OVERFLOW  LIMIT)               00
000200*/
/*                                                                      00
000210*/
/*    BRIEF OUTLINE OF ALGORITHM..                                      00
000220*/
/*                                                                      00
000230*/
/*    PHASE 1    SCANS ZERO COMPONENTS.                                 00
000240*/
/*    MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO        00
000250*/
/*    MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO                    00
000260*/
/*    MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M                  00
000270*/
/*    WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.                 00
000280*/
/*                                                                      00
000290*/
/*    VALUES FOR CUTLO AND CUTHI..                                      00
000300*/
/*    FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER    00
000310*/
/*    DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..                     00
000320*/
/*    CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE00
000330*/
/*                  UNIVAC AND DEC AT 2**(-103)                         00
000340*/
/*                  THUS CUTLO = 2**(-51) = 4.44089E-16                 00
000350*/
/*    CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.          00
000360*/
/*                  THUS CUTHI = 2**(63.5) = 1.30438E19                 00
000370*/
/*    CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.             00
000380*/
/*                  THUS CUTLO = 2**(-33.5) = 8.23181D-11               00
000390*/
/*    CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19                    00
000400*/
/*    DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /                        00
000410*/
/*    DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /                        00
000420*/
/*                                                                      00
000440*/
    if (*n > 0) {
	goto L10;
    }
    ret_val = zero;
    goto L300;
/*                                                                      00
000480*/
L10:
    next = 0;
    next_fmt = fmt_30;
    sum = zero;
    nn = *n * *incx;
/*                                                BEGIN MAIN LOOP       00
000520*/
    i = 1;
L20:
    switch ((int)next) {
	case 0: goto L30;
	case 1: goto L50;
	case 2: goto L70;
	case 3: goto L110;
    }
L30:
    if ((d__1 = sx[i], abs(d__1)) > cutlo) {
	goto L85;
    }
    next = 1;
    next_fmt = fmt_50;
    xmax = zero;
/*                                                                      00
000580*/
/*                       PHASE 1.  SUM IS ZERO                          00
000590*/
/*                                                                      00
000600*/
L50:
    if (sx[i] == zero) {
	goto L200;
    }
    if ((d__1 = sx[i], abs(d__1)) > cutlo) {
	goto L85;
    }
/*                                                                      00
000630*/
/*                               PREPARE FOR PHASE 2.                   00
000640*/
    next = 2;
    next_fmt = fmt_70;
    goto L105;
/*                                                                      00
000670*/
/*                               PREPARE FOR PHASE 4.                   00
000680*/
/*                                                                      00
000690*/
L100:
    i = j;
    next = 3;
    next_fmt = fmt_110;
    sum = sum / sx[i] / sx[i];
L105:
    xmax = (d__1 = sx[i], abs(d__1));
    goto L115;
/*                                                                      00
000750*/
/*                  PHASE 2.  SUM IS SMALL.                             00
000760*/
/*                            SCALE TO AVOID DESTRUCTIVE UNDERFLOW.     00
000770*/
/*                                                                      00
000780*/
L70:
    if ((d__1 = sx[i], abs(d__1)) > cutlo) {
	goto L75;
    }
/*                                                                      00
000800*/
/*                    COMMON CODE FOR PHASES 2 AND 4.                   00
000810*/
/*                    IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.00
000820*/
/*                                                                      00
000830*/
L110:
    if ((d__1 = sx[i], abs(d__1)) <= xmax) {
	goto L115;
    }
/* Computing 2nd power */
    d__1 = xmax / sx[i];
    sum = one + sum * (d__1 * d__1);
    xmax = (d__1 = sx[i], abs(d__1));
    goto L200;
/*                                                                      00
000880*/
L115:
/* Computing 2nd power */
    d__1 = sx[i] / xmax;
    sum += d__1 * d__1;
    goto L200;
/*                                                                      00
000910*/
/*                                                                      00
000920*/
/*                 PREPARE FOR PHASE 3.                                 00
000930*/
/*                                                                      00
000940*/
L75:
    sum = sum * xmax * xmax;
/*                                                                      00
000960*/
/*                                                                      00
000970*/
/*    FOR REAL OR D.P. SET HITEST = CUTHI/N                             00
000980*/
/*    FOR COMPLEX      SET HITEST = CUTHI/(2*N)                         00
000990*/
/*                                                                      00
001000*/
L85:
    hitest = cuthi / (doublereal) (*n);
/*                                                                      00
001020*/
/*                  PHASE 3.  SUM IS MID-RANGE.  NO SCALING.            00
001030*/
/*                                                                      00
001040*/
    i__1 = nn;
    i__2 = *incx;
    for (j = i; i__2 < 0 ? j >= i__1 : j <= i__1; j += i__2) {
	if ((d__1 = sx[j], abs(d__1)) >= hitest) {
	    goto L100;
	}
/* L95: */
/* Computing 2nd power */
	d__1 = sx[j];
	sum += d__1 * d__1;
    }
    ret_val = sqrt(sum);
    goto L300;
/*                                                                      00
001100*/
L200:
    i += *incx;
    if (i <= nn) {
	goto L20;
    }
/*                                                                      00
001140*/
/*             END OF MAIN LOOP.                                        00
001150*/
/*                                                                      00
001160*/
/*             COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.              00
001170*/
/*                                                                      00
001180*/
    ret_val = xmax * sqrt(sum);
L300:
    return ret_val;
} /* snrm2_ */

/* Subroutine */ int srot_(n, sx, incx, sy, incy, c, s)
integer *n;
doublereal *sx;
integer *incx;
doublereal *sy;
integer *incy;
doublereal *c, *s;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;
    static doublereal stemp;
    static integer ix, iy;

/*                                                                      00
000020*/
/*    APPLIES A PLANE ROTATION.                                         00
000030*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000040*/
/*                                                                      00
000050*/
/*                                                                      00
000080*/
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      00
000110*/
/*      CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL       00
000120*/
/*        TO 1                                                          00
000130*/
/*                                                                      00
000140*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	stemp = *c * sx[ix] + *s * sy[iy];
	sy[iy] = *c * sy[iy] - *s * sx[ix];
	sx[ix] = stemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      00
000270*/
/*      CODE FOR BOTH INCREMENTS EQUAL TO 1                             00
000280*/
/*                                                                      00
000290*/
L20:
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	stemp = *c * sx[i] + *s * sy[i];
	sy[i] = *c * sy[i] - *s * sx[i];
	sx[i] = stemp;
/* L30: */
    }
    return 0;
} /* srot_ */

/* Subroutine */ int srotg_(sa, sb, c, s)
doublereal *sa, *sb, *c, *s;
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double sqrt(), d_sign();

    /* Local variables */
    static doublereal r, scale, z, roe;

/*                                                                      00
000020*/
/*    CONSTRUCT GIVENS PLANE ROTATION.                                  00
000030*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000040*/
/*                                                                      00
000050*/
/*                                                                      00
000070*/
    roe = *sb;
    if (abs(*sa) > abs(*sb)) {
	roe = *sa;
    }
    scale = abs(*sa) + abs(*sb);
    if (scale != 0.) {
	goto L10;
    }
    *c = 1.;
    *s = 0.;
    r = 0.;
    goto L20;
L10:
/* Computing 2nd power */
    d__1 = *sa / scale;
/* Computing 2nd power */
    d__2 = *sb / scale;
    r = scale * sqrt(d__1 * d__1 + d__2 * d__2);
    r = d_sign(&c_b57, &roe) * r;
    *c = *sa / r;
    *s = *sb / r;
L20:
    z = 1.;
    if (abs(*sa) > abs(*sb)) {
	z = *s;
    }
    if (abs(*sb) >= abs(*sa) && *c != 0.) {
	z = 1. / *c;
    }
    *sa = r;
    *sb = z;
    return 0;
} /* srotg_ */

/* Subroutine */ int sscal_(n, sa, sx, incx)
integer *n;
doublereal *sa, *sx;
integer *incx;
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static integer i, m, nincx, mp1;

/*                                                                      00
000020*/
/*    SCALES A VECTOR BY A CONSTANT.                                    00
000030*/
/*    USES UNROLLED LOOPS FOR INCREMENT EQUAL TO 1.                     00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1) {
	goto L20;
    }
/*                                                                      00
000120*/
/*       CODE FOR INCREMENT NOT EQUAL TO 1                              00
000130*/
/*                                                                      00
000140*/
    nincx = *n * *incx;
    i__1 = nincx;
    i__2 = *incx;
    for (i = 1; i__2 < 0 ? i >= i__1 : i <= i__1; i += i__2) {
	sx[i] = *sa * sx[i];
/* L10: */
    }
    return 0;
/*                                                                      00
000200*/
/*       CODE FOR INCREMENT EQUAL TO 1                                  00
000210*/
/*                                                                      00
000220*/
/*                                                                      00
000230*/
/*       CLEAN-UP LOOP                                                  00
000240*/
/*                                                                      00
000250*/
L20:
    m = *n % 5;
    if (m == 0) {
	goto L40;
    }
    i__2 = m;
    for (i = 1; i <= i__2; ++i) {
	sx[i] = *sa * sx[i];
/* L30: */
    }
    if (*n < 5) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__2 = *n;
    for (i = mp1; i <= i__2; i += 5) {
	sx[i] = *sa * sx[i];
	sx[i + 1] = *sa * sx[i + 1];
	sx[i + 2] = *sa * sx[i + 2];
	sx[i + 3] = *sa * sx[i + 3];
	sx[i + 4] = *sa * sx[i + 4];
/* L50: */
    }
    return 0;
} /* sscal_ */

/* Subroutine */ int sswap_(n, sx, incx, sy, incy)
integer *n;
doublereal *sx;
integer *incx;
doublereal *sy;
integer *incy;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, m;
    static doublereal stemp;
    static integer ix, iy, mp1;

/*                                                                      00
000020*/
/*    INTERCHANGES TWO VECTORS.                                         00
000030*/
/*    USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO 1.                    00
000040*/
/*    JACK DONGARRA, LINPACK, 3/11/78.                                  00
000050*/
/*                                                                      00
000060*/
/*                                                                      00
000090*/
    /* Parameter adjustments */
    --sy;
    --sx;

    /* Function Body */
    if (*n <= 0) {
	return 0;
    }
    if (*incx == 1 && *incy == 1) {
	goto L20;
    }
/*                                                                      00
000120*/
/*      CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS NOT EQUAL       00
000130*/
/*        TO 1                                                          00
000140*/
/*                                                                      00
000150*/
    ix = 1;
    iy = 1;
    if (*incx < 0) {
	ix = (-(*n) + 1) * *incx + 1;
    }
    if (*incy < 0) {
	iy = (-(*n) + 1) * *incy + 1;
    }
    i__1 = *n;
    for (i = 1; i <= i__1; ++i) {
	stemp = sx[ix];
	sx[ix] = sy[iy];
	sy[iy] = stemp;
	ix += *incx;
	iy += *incy;
/* L10: */
    }
    return 0;
/*                                                                      00
000280*/
/*      CODE FOR BOTH INCREMENTS EQUAL TO 1                             00
000290*/
/*                                                                      00
000300*/
/*                                                                      00
000310*/
/*      CLEAN-UP LOOP                                                   00
000320*/
/*                                                                      00
000330*/
L20:
    m = *n % 3;
    if (m == 0) {
	goto L40;
    }
    i__1 = m;
    for (i = 1; i <= i__1; ++i) {
	stemp = sx[i];
	sx[i] = sy[i];
	sy[i] = stemp;
/* L30: */
    }
    if (*n < 3) {
	return 0;
    }
L40:
    mp1 = m + 1;
    i__1 = *n;
    for (i = mp1; i <= i__1; i += 3) {
	stemp = sx[i];
	sx[i] = sy[i];
	sy[i] = stemp;
	stemp = sx[i + 1];
	sx[i + 1] = sy[i + 1];
	sy[i + 1] = stemp;
	stemp = sx[i + 2];
	sx[i + 2] = sy[i + 2];
	sy[i + 2] = stemp;
/* L50: */
    }
    return 0;
} /* sswap_ */

