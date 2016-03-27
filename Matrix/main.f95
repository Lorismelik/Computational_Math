REAl function NORMA(B)
 REAL B(8)
 NORMA = 0.0
 DO I = 1, 8
  NORMA = NORMA + B(I)**2
 end do
 NORMA = NORMA**(0.5)
end	function

program main
    INTEGER NDIM,N,IPVT(8)
    REAl P(5) /1.0, 0.1, 0.01, 0.0001, 0.000001/
    REAL WORK(8),COND, CONDP1, TRANS(8,8), B1(8),B2(8), A(8,8), DELTA, NORMA
    REAL A_PRIM(8,8)/13.0, 7.0, -7.0, -2.0, 0.0, 0.0, -8.0, 5.0, &
            2.0, 2.0, 2.0, -8.0, 4.0, -3.0, -6.0, 5.0, &
            8.0, -4.0, 1.0, -6.0, -7.0, -6.0, -4.0, -2.0, &
            -7.0, 2.0, 3.0, -1.0, 1.0, 6.0, 7.0, -2.0, &
             7.0, 3.0, 6.0, 6.0, 22.0, 4.0, -5.0, -3.0, &
             5.0, 3.0, -6.0, 2.0, 0.0, 13.0, -5.0, 0.0, &
             -7.0, -1.0, -3.0, 1.0, -6.0, 0.0, -2.0, -7.0, &
              -7.0, -2.0, -4.0, -4.0, -6.0, 6.0, 1.0, 14.0 /
    REAL B_PRIM(8) /4.0, 36.0, -25.0 , -57.0, 32.0, 62.0, -71.0, 70.0/
    NDIM = 8
    N = 8
      DO IT=1, 5, 1
      B1=B_PRIM
      A=A_PRIM
      A (1,1) = A (1,1) + P(IT)
      B1 (1) = B1 (1)*P(IT)+6
      CALL DECOMP(NDIM,N,A,COND,IPVT,WORK)
      PRINT 101,((A(I,J),J=1,N),I=1,N)
      PRINT 102,COND
      CONDP1=COND+1.0
      IF(CONDP1.EQ.COND) PRINT 103
      IF(CONDP1.NE.COND) CALL SOLVE(NDIM,N,A,B1,IPVT)
      IF(CONDP1.NE.COND) PRINT 104,(B1(I),I=1,N)

      B2=B_PRIM
      A=A_PRIM
      A (1,1) = A (1,1) + P(IT)
      B2 (1) = B2 (1)*P(IT)+6
      TRANS = TRANSPOSE(A)
      A = MATMUL(TRANS, A)
      B2 = MATMUL(TRANS, B2)
      CALL DECOMP(NDIM,N,A,COND,IPVT,WORK)
      PRINT 106,((A(I,J),J=1,N),I=1,N)
      PRINT 102,COND
      CONDP1=COND+1.0
      IF(CONDP1.EQ.COND) PRINT 103
      IF(CONDP1.NE.COND) CALL SOLVE(NDIM,N,A,B2,IPVT)
      IF(CONDP1.NE.COND) PRINT 104,(B2(I),I=1,N)
      IF(CONDP1.NE.COND) DELTA = NORMA(B1 - B2)/NORMA(B1)
      IF(CONDP1.NE.COND) PRINT 105,DELTA
      end do

  101 FORMAT(13X,'A',8(/5X,8F8.3))
  102 FORMAT(5X,'COND = ',E12.5)
  103 FORMAT(5X,'Matrix classified as degenerate')
  104 FORMAT(5X,'Solution X',3(/12X,F10.7))
  105 FORMAT(5X,'Delta = ', E12.5)
  106 FORMAT(13X,'A1',8(/5X,8F8.3))
      end
