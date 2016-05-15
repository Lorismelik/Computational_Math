SUBROUTINE F(T,Y,YP)
      REAL Y(2),YP(2),C1,C2
       COMMON /cm1/ C1, C2
      YP(1)= Y(2)
      YP(2)=C1*Y(1)/((1+Y(1)*Y(1))**3)-C2*Y(2)
      RETURN
      END
      program main
      implicit none
      EXTERNAL F
      external integral
      external fzeroin
      external ZEROIN
      REAL T,Y(2),TOUT,RELERR,ABSERR, TFINAL,TPRINT,WORK(15), A, B, FLAG, integral, RESULT, ERREST,C1, C2, YP(2), TOL, ZEROIN, Z, fzeroin
      INTEGER IWORK(5),IFLAG, NOFUN
      INTEGER, PARAMETER :: NEQN = 2
      COMMON /cm1/ C1, C2
      T=0.0
      Y(1)=0.05
      Y(2)=0.0
      RELERR=1.0E-4
      ABSERR=0.0
      TFINAL=1.0
      TPRINT=0.01
      IFLAG=1
      TOUT=T
      A=-1.0
      B=0.0
      TOL=1.0E-7
      Z = ZEROIN(A,B,fzeroin ,TOL)
      C2 = -2.493594*Z
      B = 1.570796326794896619231321
      A = 0
    CALL QUANC8(integral,A,B,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
    C1 = 0.6087475 * RESULT
     OPEN(0, FILE = 'RKF45.txt')
   10 CALL RKF45(F,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
      WRITE(0, 11) T,Y(1)
	  GO TO (80,20,30,40,50,60,70,80),IFLAG
   20 TOUT=TPRINT+T
      IF(T.LT.TFINAL) GO TO 10
      STOP
   30 PRINT 31,RELERR,ABSERR
      GO TO 10
   40 PRINT 41
      GO TO 10
   50 ABSERR=0.1E-07
      PRINT 31,RELERR,ABSERR
      GO TO 10
   60 RELERR=RELERR*10.0
      PRINT 31,RELERR,ABSERR
      IFLAG=2
      GO TO 10
   70 PRINT 71
      IFLAG=2
      GO TO 10
   80 PRINT 81
      STOP
   11 FORMAT(' T=',F5.2,2X,'Y=',F10.6,2X)
   31 FORMAT(' Errors boundaries have changed  '/' RELERR=',E10.3,2X,'ABSERR=',E10.3)
   41 FORMAT(' Many steps ')
   71 FORMAT(' Many outlets ')
   81 FORMAT(' Irregular call ')
      end
real function fzeroin (x)
fzeroin = exp(x) + exp(-3*x) -4
end function
REAl function integral (x)
integral =  x/(sin(x)*(cos(x)+0.8*sin(x)))
end function
