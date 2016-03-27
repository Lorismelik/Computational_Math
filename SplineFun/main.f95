REAl function FUN (U)
INTEGER N,I
REAL X(11),Y(11),B(11),C(11),D(11),SEVAl, U
 N = 11
  DO I=1,N
        X(i) = 0.3*i
        Y(i) = 1-exp(-X(i))
 ENDDO
CALL SPLINE (N,X,Y,B,C,D)
S=SEVAL(N,U,X,Y,B,C,D)
FUN = S
PRINT*, U, S

end function FUN

program main
implicit none
EXTERNAL FUN
  INTEGER N,I
  REAL X(11),Y(11),B(11),C(11),D(11),SEVAl, U, LEFT, RIGHT, ABSERR,RELERR, RESULT,ERREST,FLAG,S
  INTEGER NOFUN
  N = 11
  DO I=1,N
        X(i) = 0.3*i
        Y(i) = 1-exp(-X(i))
 ENDDO
      CALL SPLINE (N,X,Y,B,C,D)
      PRINT 100
      PRINT 101,(B(I),C(I),D(I),I=1,N)
      U = 2.5
      S=SEVAL(N,U,X,Y,B,C,D)
      PRINT 102,U,S
      LEFT = 0
      RIGHT = 3
      RELERR = 1.E-07
      ABSERR = 0
      CALL QUANC8(FUN,LEFT,RIGHT,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
      PRINT *, RESULT, ERREST, NOFUN, FLAG
  100 FORMAT(14X,'B',15X,'C',15X,'D')
  101 FORMAT(5X,3E16.7)
  102 FORMAT(14X,'U=',F3.1,5X,'S=',F10.7)
      END
