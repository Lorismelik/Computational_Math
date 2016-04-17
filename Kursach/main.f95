
SUBROUTINE FUN(T,Y,YP)
      REAL E,Y(2),YP(2)
      COMMON /cm1/E
      YP(1)=(-1+E*Y(2)*Y(2))*Y(2)
      YP(2)= Y(1)
      RETURN
      END
REAl function integral (x)
integral =  exp(-2.5*x*x)/(1+sin(2.5*x))
end function
      program main
      implicit none
      EXTERNAL FUN
      external integral
      REAL T,Y(2),TOUT,RELERR,ABSERR, TFINAL,TPRINT,WORK(15),E , A, B, FLAG, integral, RESULT, ERREST
      INTEGER IWORK(5),IFLAG, NOFUN
      INTEGER, PARAMETER :: NEQN = 2
      COMMON /cm1/E
      T=0.0
      Y(1)=0.0
      Y(2)=0.999998963969
      RELERR=1.0E-4
      ABSERR=0.0
      TFINAL=16
      TPRINT=0.4
      IFLAG=1
      TOUT=T
      A = 0
      B = 1
      CALL QUANC8(integral,A,B,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
      E = 0.5641653 * RESULT
      OPEN(0, FILE = 'RKF45.txt')
   10 CALL RKF45(FUN,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
      Y(2)= Y(2)+RELERR
      WRITE(0, 11)Y(2)
	  WRITE(3, *) Y(2)

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
   11 FORMAT(F15.6)
   31 FORMAT(' Errors boundaries have changed  '/' RELERR=',E10.3,2X,'ABSERR=',E10.3)
   41 FORMAT(' Many steps ')
   71 FORMAT(' Many outlets ')
   81 FORMAT(' Irregular call ')
      end
