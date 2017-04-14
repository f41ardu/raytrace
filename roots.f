      SUBROUTINE LAGUER(A,M,X,EPS,POLISH)
      COMPLEX A(*),X,DX,X1,B,D,F,G,H,SQ,GP,GM,G2,ZERO
      LOGICAL POLISH
      PARAMETER (ZERO=(0.,0.),EPSS=6.E-8,MAXIT=100)
      DXOLD=CABS(X)
      DO 12 ITER=1,MAXIT
        B=A(M+1)
        ERR=CABS(B)
        D=ZERO
        F=ZERO
        ABX=CABS(X)
        DO 11 J=M,1,-1
          F=X*F+D
          D=X*D+B
          B=X*B+A(J)
          ERR=CABS(B)+ABX*ERR
11      CONTINUE
        ERR=EPSS*ERR
        IF(CABS(B).LE.ERR) THEN
          DX=ZERO
          RETURN
        ELSE
          G=D/B
          G2=G*G
          H=G2-2.*F/B
          SQ=CSQRT((M-1)*(M*H-G2))
          GP=G+SQ
          GM=G-SQ
          IF(CABS(GP).LT.CABS(GM)) GP=GM
          DX=M/GP
        ENDIF
        X1=X-DX
        IF(X.EQ.X1)RETURN
        X=X1
        CDX=CABS(DX)
        IF(ITER.GT.6.AND.CDX.GE.DXOLD)RETURN
        DXOLD=CDX
        IF(.NOT.POLISH)THEN
          IF(CABS(DX).LE.EPS*CABS(X))RETURN
        ENDIF
12    CONTINUE
      write (6,*) 'too many iterations'
      RETURN
      END
c 
      SUBROUTINE ZROOTS(A,M,ROOTS,POLISH)
      PARAMETER (EPS=1.E-6,MAXM=101)
      COMPLEX A(*),ROOTS(M),AD(MAXM),X,B,C
      LOGICAL POLISH
      DO 11 J=1,M+1
        AD(J)=A(J)
11    CONTINUE
      DO 13 J=M,1,-1
        X=CMPLX(0.,0.)
        CALL LAGUER(AD,J,X,EPS,.FALSE.)
        IF(ABS(AIMAG(X)).LE.2.*EPS**2*ABS(REAL(X))) X=CMPLX(REAL(X),0.)
        ROOTS(J)=X
        B=AD(J+1)
        DO 12 JJ=J,1,-1
          C=AD(JJ)
          AD(JJ)=B
          B=X*B+C
12      CONTINUE
13    CONTINUE
      IF (POLISH) THEN
        DO 14 J=1,M
          CALL LAGUER(A,M,ROOTS(J),EPS,.TRUE.)
14      CONTINUE
      ENDIF
      DO 16 J=2,M
        X=ROOTS(J)
        DO 15 I=J-1,1,-1
          IF(REAL(ROOTS(I)).LE.REAL(X))GO TO 10
          ROOTS(I+1)=ROOTS(I)
15      CONTINUE
        I=0
10      ROOTS(I+1)=X
16    CONTINUE
      RETURN
      END
