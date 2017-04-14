c
      FUNCTION GASDEV(IDUM)
      real gasdev,ran3
      DATA ISET/0/
      IF (ISET.EQ.0) THEN
1       V1=2.0*ran3(1)-1.0
        V2=2.0*ran3(1)-1.0
        R=V1**2+V2**2
        IF(R.GE.1.)GO TO 1
        FAC=SQRT(-2.0*LOG(R)/R)
        GSET=V1*FAC
        GASDEV=V2*FAC
        ISET=1
      ELSE
        GASDEV=GSET
        ISET=0
      ENDIF
      RETURN
      END
c
