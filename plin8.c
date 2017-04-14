/* plin8.f -- translated by f2c (version of 23 April 1993  18:34:30).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static doublecomplex c_b48 = {-1.,0.};

/* Subroutine */ int cgeco_(a, lda, n, ipvt, rcond, z)
doublecomplex *a;
integer *lda, *n, *ipvt;
doublereal *rcond;
doublecomplex *z;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2, d__3, d__4, d__5, d__6, d__7, d__8;
    doublecomplex z__1, z__2, z__3, z__4;

    /* Builtin functions */
    double d_imag();
    void d_cnjg(), z_div();

    /* Local variables */
    static integer info;
    extern /* Subroutine */ int cgefa_();
    static integer j, k, l;
    static doublereal s;
    static doublecomplex t;
    extern /* Double Complex */ int cdotc_();
    static doublereal anorm;
    extern /* Subroutine */ int caxpy_();
    static doublereal ynorm;
    static integer kb;
    static doublecomplex ek;
    static doublereal sm;
    static doublecomplex wk;
    extern /* Subroutine */ int csscal_();
    extern doublereal scasum_();
    static integer kp1;
    static doublecomplex wkm;


/*     CGECO FACTORS A COMPLEX MATRIX BY GAUSSIAN ELIMINATION */
/*     AND ESTIMATES THE CONDITION OF THE MATRIX. */

/*     IF  RCOND  IS NOT NEEDED, CGEFA IS SLIGHTLY FASTER. */
/*     TO SOLVE  A*X = B , FOLLOW CGECO BY CGESL. */
/*     TO COMPUTE  INVERSE(A)*C , FOLLOW CGECO BY CGESL. */
/*     TO COMPUTE  DETERMINANT(A) , FOLLOW CGECO BY CGEDI. */
/*     TO COMPUTE  INVERSE(A) , FOLLOW CGECO BY CGEDI. */

/*     ON ENTRY */

/*        A       COMPLEX(LDA, N) */
/*                THE MATRIX TO BE FACTORED. */

/*        LDA     INTEGER */
/*                THE LEADING DIMENSION OF THE ARRAY  A . */

/*        N       INTEGER */
/*                THE ORDER OF THE MATRIX  A . */

/*     ON RETURN */

/*        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS */
/*                WHICH WERE USED TO OBTAIN IT. */
/*                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE */
/*                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER */
/*                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR. */

/*        IPVT    INTEGER(N) */
/*                AN INTEGER VECTOR OF PIVOT INDICES. */

/*        RCOND   REAL */
/*                AN ESTIMATE OF THE RECIPROCAL CONDITION OF  A . */
/*                FOR THE SYSTEM  A*X = B , RELATIVE PERTURBATIONS */
/*                IN  A  AND  B  OF SIZE  EPSILON  MAY CAUSE */
/*                RELATIVE PERTURBATIONS IN  X  OF SIZE  EPSILON/RCOND . 
*/
/*                IF  RCOND  IS SO SMALL THAT THE LOGICAL EXPRESSION */
/*                           1.0 + RCOND .EQ. 1.0 */
/*                IS TRUE, THEN  A  MAY BE SINGULAR TO WORKING */
/*                PRECISION.  IN PARTICULAR,  RCOND  IS ZERO  IF */
/*                EXACT SINGULARITY IS DETECTED OR THE ESTIMATE */
/*                UNDERFLOWS. */

/*        Z       COMPLEX(N) */
/*                A WORK VECTOR WHOSE CONTENTS ARE USUALLY UNIMPORTANT. */
/*                IF  A  IS CLOSE TO A SINGULAR MATRIX, THEN  Z  IS */
/*                AN APPROXIMATE NULL VECTOR IN THE SENSE THAT */
/*                NORM(A*Z) = RCOND*NORM(A)*NORM(Z) . */

/*     LINPACK. THIS VERSION DATED 08/14/78 . */
/*     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. */

/*     SUBROUTINES AND FUNCTIONS */

/*     LINPACK CGEFA */
/*     BLAS CAXPY,CDOTC,CSSCAL,SCASUM */
/*     FORTRAN ABS,AIMAG,AMAX1,CMPLX,CONJG,REAL */

/*     INTERNAL VARIABLES */



/*     COMPUTE 1-NORM OF A */

    /* Parameter adjustments */
    --z;
    --ipvt;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    anorm = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
/* Computing MAX */
	d__1 = anorm, d__2 = scasum_(n, &a[j * a_dim1 + 1], &c__1);
	anorm = max(d__1,d__2);
/* L10: */
    }

/*     FACTOR */

    cgefa_(&a[a_offset], lda, n, &ipvt[1], &info);

/*     RCOND = 1/(NORM(A)*(ESTIMATE OF NORM(INVERSE(A)))) . */
/*     ESTIMATE = NORM(Z)/NORM(Y) WHERE  A*Z = Y  AND  CTRANS(A)*Y = E . 
*/
/*     CTRANS(A)  IS THE CONJUGATE TRANSPOSE OF A . */
/*     THE COMPONENTS OF  E  ARE CHOSEN TO CAUSE MAXIMUM LOCAL */
/*     GROWTH IN THE ELEMENTS OF W  WHERE  CTRANS(U)*W = E . */
/*     THE VECTORS ARE FREQUENTLY RESCALED TO AVOID OVERFLOW. */

/*     SOLVE CTRANS(U)*W = E */

    ek.r = 1., ek.i = 0.;
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = j;
	z[i__2].r = 0., z[i__2].i = 0.;
/* L20: */
    }
    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k;
	if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(d__2)) 
		!= 0.) {
	    i__3 = k;
	    z__2.r = -z[i__3].r, z__2.i = -z[i__3].i;
	    z__1.r = z__2.r, z__1.i = z__2.i;
	    d__7 = (d__3 = ek.r, abs(d__3)) + (d__4 = d_imag(&ek), abs(d__4));
	    d__8 = (d__5 = z__1.r, abs(d__5)) + (d__6 = d_imag(&z__1), abs(
		    d__6));
	    z__4.r = z__1.r / d__8, z__4.i = z__1.i / d__8;
	    z__3.r = d__7 * z__4.r, z__3.i = d__7 * z__4.i;
	    ek.r = z__3.r, ek.i = z__3.i;
	}
	i__2 = k;
	z__2.r = ek.r - z[i__2].r, z__2.i = ek.i - z[i__2].i;
	z__1.r = z__2.r, z__1.i = z__2.i;
	i__3 = k + k * a_dim1;
	if ((d__1 = z__1.r, abs(d__1)) + (d__2 = d_imag(&z__1), abs(d__2)) <= 
		(d__3 = a[i__3].r, abs(d__3)) + (d__4 = d_imag(&a[k + k * 
		a_dim1]), abs(d__4))) {
	    goto L30;
	}
	i__2 = k;
	z__2.r = ek.r - z[i__2].r, z__2.i = ek.i - z[i__2].i;
	z__1.r = z__2.r, z__1.i = z__2.i;
	i__3 = k + k * a_dim1;
	s = ((d__1 = a[i__3].r, abs(d__1)) + (d__2 = d_imag(&a[k + k * a_dim1]
		), abs(d__2))) / ((d__3 = z__1.r, abs(d__3)) + (d__4 = d_imag(
		&z__1), abs(d__4)));
	csscal_(n, &s, &z[1], &c__1);
	z__2.r = s, z__2.i = 0.;
	z__1.r = z__2.r * ek.r - z__2.i * ek.i, z__1.i = z__2.r * ek.i + 
		z__2.i * ek.r;
	ek.r = z__1.r, ek.i = z__1.i;
L30:
	i__2 = k;
	z__1.r = ek.r - z[i__2].r, z__1.i = ek.i - z[i__2].i;
	wk.r = z__1.r, wk.i = z__1.i;
	z__2.r = -ek.r, z__2.i = -ek.i;
	i__2 = k;
	z__1.r = z__2.r - z[i__2].r, z__1.i = z__2.i - z[i__2].i;
	wkm.r = z__1.r, wkm.i = z__1.i;
	s = (d__1 = wk.r, abs(d__1)) + (d__2 = d_imag(&wk), abs(d__2));
	sm = (d__1 = wkm.r, abs(d__1)) + (d__2 = d_imag(&wkm), abs(d__2));
	i__2 = k + k * a_dim1;
	if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = d_imag(&a[k + k * a_dim1])
		, abs(d__2)) == 0.) {
	    goto L40;
	}
	d_cnjg(&z__2, &a[k + k * a_dim1]);
	z_div(&z__1, &wk, &z__2);
	wk.r = z__1.r, wk.i = z__1.i;
	d_cnjg(&z__2, &a[k + k * a_dim1]);
	z_div(&z__1, &wkm, &z__2);
	wkm.r = z__1.r, wkm.i = z__1.i;
	goto L50;
L40:
	wk.r = 1., wk.i = 0.;
	wkm.r = 1., wkm.i = 0.;
L50:
	kp1 = k + 1;
	if (kp1 > *n) {
	    goto L90;
	}
	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    i__3 = j;
	    d_cnjg(&z__4, &a[k + j * a_dim1]);
	    z__3.r = wkm.r * z__4.r - wkm.i * z__4.i, z__3.i = wkm.r * z__4.i 
		    + wkm.i * z__4.r;
	    z__2.r = z[i__3].r + z__3.r, z__2.i = z[i__3].i + z__3.i;
	    z__1.r = z__2.r, z__1.i = z__2.i;
	    sm += (d__1 = z__1.r, abs(d__1)) + (d__2 = d_imag(&z__1), abs(
		    d__2));
	    i__3 = j;
	    i__4 = j;
	    d_cnjg(&z__3, &a[k + j * a_dim1]);
	    z__2.r = wk.r * z__3.r - wk.i * z__3.i, z__2.i = wk.r * z__3.i + 
		    wk.i * z__3.r;
	    z__1.r = z[i__4].r + z__2.r, z__1.i = z[i__4].i + z__2.i;
	    z[i__3].r = z__1.r, z[i__3].i = z__1.i;
	    i__3 = j;
	    s += (d__1 = z[i__3].r, abs(d__1)) + (d__2 = d_imag(&z[j]), abs(
		    d__2));
/* L60: */
	}
	if (s >= sm) {
	    goto L80;
	}
	z__1.r = wkm.r - wk.r, z__1.i = wkm.i - wk.i;
	t.r = z__1.r, t.i = z__1.i;
	wk.r = wkm.r, wk.i = wkm.i;
	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    i__3 = j;
	    i__4 = j;
	    d_cnjg(&z__3, &a[k + j * a_dim1]);
	    z__2.r = t.r * z__3.r - t.i * z__3.i, z__2.i = t.r * z__3.i + t.i 
		    * z__3.r;
	    z__1.r = z[i__4].r + z__2.r, z__1.i = z[i__4].i + z__2.i;
	    z[i__3].r = z__1.r, z[i__3].i = z__1.i;
/* L70: */
	}
L80:
L90:
	i__2 = k;
	z[i__2].r = wk.r, z[i__2].i = wk.i;
/* L100: */
    }
    s = 1. / scasum_(n, &z[1], &c__1);
    csscal_(n, &s, &z[1], &c__1);

/*     SOLVE CTRANS(L)*Y = W */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	if (k < *n) {
	    i__2 = k;
	    i__3 = k;
	    i__4 = *n - k;
	    cdotc_(&z__2, &i__4, &a[k + 1 + k * a_dim1], &c__1, &z[k + 1], &
		    c__1);
	    z__1.r = z[i__3].r + z__2.r, z__1.i = z[i__3].i + z__2.i;
	    z[i__2].r = z__1.r, z[i__2].i = z__1.i;
	}
	i__2 = k;
	if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(d__2)) 
		<= 1.) {
	    goto L110;
	}
	i__2 = k;
	s = 1. / ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(
		d__2)));
	csscal_(n, &s, &z[1], &c__1);
L110:
	l = ipvt[k];
	i__2 = l;
	t.r = z[i__2].r, t.i = z[i__2].i;
	i__2 = l;
	i__3 = k;
	z[i__2].r = z[i__3].r, z[i__2].i = z[i__3].i;
	i__2 = k;
	z[i__2].r = t.r, z[i__2].i = t.i;
/* L120: */
    }
    s = 1. / scasum_(n, &z[1], &c__1);
    csscal_(n, &s, &z[1], &c__1);

    ynorm = 1.;

/*     SOLVE L*V = Y */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	l = ipvt[k];
	i__2 = l;
	t.r = z[i__2].r, t.i = z[i__2].i;
	i__2 = l;
	i__3 = k;
	z[i__2].r = z[i__3].r, z[i__2].i = z[i__3].i;
	i__2 = k;
	z[i__2].r = t.r, z[i__2].i = t.i;
	if (k < *n) {
	    i__2 = *n - k;
	    caxpy_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1, &z[k + 1], &c__1)
		    ;
	}
	i__2 = k;
	if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(d__2)) 
		<= 1.) {
	    goto L130;
	}
	i__2 = k;
	s = 1. / ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(
		d__2)));
	csscal_(n, &s, &z[1], &c__1);
	ynorm = s * ynorm;
L130:
/* L140: */
	;
    }
    s = 1. / scasum_(n, &z[1], &c__1);
    csscal_(n, &s, &z[1], &c__1);
    ynorm = s * ynorm;

/*     SOLVE  U*Z = V */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	i__2 = k;
	i__3 = k + k * a_dim1;
	if ((d__1 = z[i__2].r, abs(d__1)) + (d__2 = d_imag(&z[k]), abs(d__2)) 
		<= (d__3 = a[i__3].r, abs(d__3)) + (d__4 = d_imag(&a[k + k * 
		a_dim1]), abs(d__4))) {
	    goto L150;
	}
	i__2 = k + k * a_dim1;
	i__3 = k;
	s = ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = d_imag(&a[k + k * a_dim1]
		), abs(d__2))) / ((d__3 = z[i__3].r, abs(d__3)) + (d__4 = 
		d_imag(&z[k]), abs(d__4)));
	csscal_(n, &s, &z[1], &c__1);
	ynorm = s * ynorm;
L150:
	i__2 = k + k * a_dim1;
	if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = d_imag(&a[k + k * a_dim1])
		, abs(d__2)) != 0.) {
	    i__3 = k;
	    z_div(&z__1, &z[k], &a[k + k * a_dim1]);
	    z[i__3].r = z__1.r, z[i__3].i = z__1.i;
	}
	i__2 = k + k * a_dim1;
	if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = d_imag(&a[k + k * a_dim1])
		, abs(d__2)) == 0.) {
	    i__3 = k;
	    z[i__3].r = 1., z[i__3].i = 0.;
	}
	i__2 = k;
	z__1.r = -z[i__2].r, z__1.i = -z[i__2].i;
	t.r = z__1.r, t.i = z__1.i;
	i__2 = k - 1;
	caxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &z[1], &c__1);
/* L160: */
    }
/*     MAKE ZNORM = 1.0 */
    s = 1. / scasum_(n, &z[1], &c__1);
    csscal_(n, &s, &z[1], &c__1);
    ynorm = s * ynorm;

    if (anorm != 0.) {
	*rcond = ynorm / anorm;
    }
    if (anorm == 0.) {
	*rcond = 0.;
    }
    return 0;
} /* cgeco_ */

/* Subroutine */ int cgefa_(a, lda, n, ipvt, info)
doublecomplex *a;
integer *lda, *n, *ipvt, *info;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublereal d__1, d__2;
    doublecomplex z__1;

    /* Builtin functions */
    double d_imag();
    void z_div();

    /* Local variables */
    static integer j, k, l;
    static doublecomplex t;
    extern /* Subroutine */ int cscal_(), caxpy_();
    extern integer icamax_();
    static integer kp1, nm1;


/*     CGEFA FACTORS A COMPLEX MATRIX BY GAUSSIAN ELIMINATION. */

/*     CGEFA IS USUALLY CALLED BY CGECO, BUT IT CAN BE CALLED */
/*     DIRECTLY WITH A SAVING IN TIME IF  RCOND  IS NOT NEEDED. */
/*     (TIME FOR CGECO) = (1 + 9/N)*(TIME FOR CGEFA) . */

/*     ON ENTRY */

/*        A       COMPLEX(LDA, N) */
/*                THE MATRIX TO BE FACTORED. */

/*        LDA     INTEGER */
/*                THE LEADING DIMENSION OF THE ARRAY  A . */

/*        N       INTEGER */
/*                THE ORDER OF THE MATRIX  A . */

/*     ON RETURN */

/*        A       AN UPPER TRIANGULAR MATRIX AND THE MULTIPLIERS */
/*                WHICH WERE USED TO OBTAIN IT. */
/*                THE FACTORIZATION CAN BE WRITTEN  A = L*U  WHERE */
/*                L  IS A PRODUCT OF PERMUTATION AND UNIT LOWER */
/*                TRIANGULAR MATRICES AND  U  IS UPPER TRIANGULAR. */

/*        IPVT    INTEGER(N) */
/*                AN INTEGER VECTOR OF PIVOT INDICES. */

/*        INFO    INTEGER */
/*                = 0  NORMAL VALUE. */
/*                = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR */
/*                     CONDITION FOR THIS SUBROUTINE, BUT IT DOES */
/*                     INDICATE THAT CGESL OR CGEDI WILL DIVIDE BY ZERO */
/*                     IF CALLED.  USE  RCOND  IN CGECO FOR A RELIABLE */
/*                     INDICATION OF SINGULARITY. */

/*     LINPACK. THIS VERSION DATED 08/14/78 . */
/*     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. */

/*     SUBROUTINES AND FUNCTIONS */

/*     BLAS CAXPY,CSCAL,ICAMAX */
/*     FORTRAN ABS,AIMAG,REAL */

/*     INTERNAL VARIABLES */



/*     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */

    /* Parameter adjustments */
    --ipvt;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    *info = 0;
    nm1 = *n - 1;
    if (nm1 < 1) {
	goto L70;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	kp1 = k + 1;

/*        FIND L = PIVOT INDEX */

	i__2 = *n - k + 1;
	l = icamax_(&i__2, &a[k + k * a_dim1], &c__1) + k - 1;
	ipvt[k] = l;

/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED */

	i__2 = l + k * a_dim1;
	if ((d__1 = a[i__2].r, abs(d__1)) + (d__2 = d_imag(&a[l + k * a_dim1])
		, abs(d__2)) == 0.) {
	    goto L40;
	}

/*           INTERCHANGE IF NECESSARY */

	if (l == k) {
	    goto L10;
	}
	i__2 = l + k * a_dim1;
	t.r = a[i__2].r, t.i = a[i__2].i;
	i__2 = l + k * a_dim1;
	i__3 = k + k * a_dim1;
	a[i__2].r = a[i__3].r, a[i__2].i = a[i__3].i;
	i__2 = k + k * a_dim1;
	a[i__2].r = t.r, a[i__2].i = t.i;
L10:

/*           COMPUTE MULTIPLIERS */

	z_div(&z__1, &c_b48, &a[k + k * a_dim1]);
	t.r = z__1.r, t.i = z__1.i;
	i__2 = *n - k;
	cscal_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1);

/*           ROW ELIMINATION WITH COLUMN INDEXING */

	i__2 = *n;
	for (j = kp1; j <= i__2; ++j) {
	    i__3 = l + j * a_dim1;
	    t.r = a[i__3].r, t.i = a[i__3].i;
	    if (l == k) {
		goto L20;
	    }
	    i__3 = l + j * a_dim1;
	    i__4 = k + j * a_dim1;
	    a[i__3].r = a[i__4].r, a[i__3].i = a[i__4].i;
	    i__3 = k + j * a_dim1;
	    a[i__3].r = t.r, a[i__3].i = t.i;
L20:
	    i__3 = *n - k;
	    caxpy_(&i__3, &t, &a[k + 1 + k * a_dim1], &c__1, &a[k + 1 + j * 
		    a_dim1], &c__1);
/* L30: */
	}
	goto L50;
L40:
	*info = k;
L50:
/* L60: */
	;
    }
L70:
    ipvt[*n] = *n;
    i__1 = *n + *n * a_dim1;
    if ((d__1 = a[i__1].r, abs(d__1)) + (d__2 = d_imag(&a[*n + *n * a_dim1]), 
	    abs(d__2)) == 0.) {
	*info = *n;
    }
    return 0;
} /* cgefa_ */

/* Subroutine */ int cgesl_(a, lda, n, ipvt, b, job)
doublecomplex *a;
integer *lda, *n, *ipvt;
doublecomplex *b;
integer *job;
{
    /* System generated locals */
    integer a_dim1, a_offset, i__1, i__2, i__3, i__4;
    doublecomplex z__1, z__2, z__3;

    /* Builtin functions */
    void z_div(), d_cnjg();

    /* Local variables */
    static integer k, l;
    static doublecomplex t;
    extern /* Double Complex */ int cdotc_();
    extern /* Subroutine */ int caxpy_();
    static integer kb, nm1;


/*     CGESL SOLVES THE COMPLEX SYSTEM */
/*     A * X = B  OR  CTRANS(A) * X = B */
/*     USING THE FACTORS COMPUTED BY CGECO OR CGEFA. */

/*     ON ENTRY */

/*        A       COMPLEX(LDA, N) */
/*                THE OUTPUT FROM CGECO OR CGEFA. */

/*        LDA     INTEGER */
/*                THE LEADING DIMENSION OF THE ARRAY  A . */

/*        N       INTEGER */
/*                THE ORDER OF THE MATRIX  A . */

/*        IPVT    INTEGER(N) */
/*                THE PIVOT VECTOR FROM CGECO OR CGEFA. */

/*        B       COMPLEX(N) */
/*                THE RIGHT HAND SIDE VECTOR. */

/*        JOB     INTEGER */
/*                = 0         TO SOLVE  A*X = B , */
/*                = NONZERO   TO SOLVE  CTRANS(A)*X = B  WHERE */
/*                            CTRANS(A)  IS THE CONJUGATE TRANSPOSE. */

/*     ON RETURN */

/*        B       THE SOLUTION VECTOR  X . */

/*     ERROR CONDITION */

/*        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A */
/*        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY */
/*        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER */
/*        SETTING OF LDA .  IT WILL NOT OCCUR IF THE SUBROUTINES ARE */
/*        CALLED CORRECTLY AND IF CGECO HAS SET RCOND .GT. 0.0 */
/*        OR CGEFA HAS SET INFO .EQ. 0 . */

/*     TO COMPUTE  INVERSE(A) * C  WHERE  C  IS A MATRIX */
/*     WITH  P  COLUMNS */
/*           CALL CGECO(A,LDA,N,IPVT,RCOND,Z) */
/*           IF (RCOND IS TOO SMALL) GO TO ... */
/*           DO 10 J = 1, P */
/*              CALL CGESL(A,LDA,N,IPVT,C(1,J),0) */
/*        10 CONTINUE */

/*     LINPACK. THIS VERSION DATED 08/14/78 . */
/*     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB. */

/*     SUBROUTINES AND FUNCTIONS */

/*     BLAS CAXPY,CDOTC */
/*     FORTRAN CONJG */

/*     INTERNAL VARIABLES */


    /* Parameter adjustments */
    --b;
    --ipvt;
    a_dim1 = *lda;
    a_offset = a_dim1 + 1;
    a -= a_offset;

    /* Function Body */
    nm1 = *n - 1;
    if (*job != 0) {
	goto L50;
    }

/*        JOB = 0 , SOLVE  A * X = B */
/*        FIRST SOLVE  L*Y = B */

    if (nm1 < 1) {
	goto L30;
    }
    i__1 = nm1;
    for (k = 1; k <= i__1; ++k) {
	l = ipvt[k];
	i__2 = l;
	t.r = b[i__2].r, t.i = b[i__2].i;
	if (l == k) {
	    goto L10;
	}
	i__2 = l;
	i__3 = k;
	b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
	i__2 = k;
	b[i__2].r = t.r, b[i__2].i = t.i;
L10:
	i__2 = *n - k;
	caxpy_(&i__2, &t, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
/* L20: */
    }
L30:

/*        NOW SOLVE  U*X = Y */

    i__1 = *n;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n + 1 - kb;
	i__2 = k;
	z_div(&z__1, &b[k], &a[k + k * a_dim1]);
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	i__2 = k;
	z__1.r = -b[i__2].r, z__1.i = -b[i__2].i;
	t.r = z__1.r, t.i = z__1.i;
	i__2 = k - 1;
	caxpy_(&i__2, &t, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
/* L40: */
    }
    goto L100;
L50:

/*        JOB = NONZERO, SOLVE  CTRANS(A) * X = B */
/*        FIRST SOLVE  CTRANS(U)*Y = B */

    i__1 = *n;
    for (k = 1; k <= i__1; ++k) {
	i__2 = k - 1;
	cdotc_(&z__1, &i__2, &a[k * a_dim1 + 1], &c__1, &b[1], &c__1);
	t.r = z__1.r, t.i = z__1.i;
	i__2 = k;
	i__3 = k;
	z__2.r = b[i__3].r - t.r, z__2.i = b[i__3].i - t.i;
	d_cnjg(&z__3, &a[k + k * a_dim1]);
	z_div(&z__1, &z__2, &z__3);
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
/* L60: */
    }

/*        NOW SOLVE CTRANS(L)*X = Y */

    if (nm1 < 1) {
	goto L90;
    }
    i__1 = nm1;
    for (kb = 1; kb <= i__1; ++kb) {
	k = *n - kb;
	i__2 = k;
	i__3 = k;
	i__4 = *n - k;
	cdotc_(&z__2, &i__4, &a[k + 1 + k * a_dim1], &c__1, &b[k + 1], &c__1);
	z__1.r = b[i__3].r + z__2.r, z__1.i = b[i__3].i + z__2.i;
	b[i__2].r = z__1.r, b[i__2].i = z__1.i;
	l = ipvt[k];
	if (l == k) {
	    goto L70;
	}
	i__2 = l;
	t.r = b[i__2].r, t.i = b[i__2].i;
	i__2 = l;
	i__3 = k;
	b[i__2].r = b[i__3].r, b[i__2].i = b[i__3].i;
	i__2 = k;
	b[i__2].r = t.r, b[i__2].i = t.i;
L70:
/* L80: */
	;
    }
L90:
L100:
    return 0;
} /* cgesl_ */

