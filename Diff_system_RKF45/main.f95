
      SUBROUTINE FUN(T,Y,YP)
      REAL T,Y(2),YP(2)
      YP(1)=-130.0*Y(1)+900.0*Y(2)+exp(-10.0*T)
      YP(2)=30.0*Y(1)-300.0*Y(2)+log(1.0+100.0*T**2)
      RETURN
      END
      program main
      EXTERNAL FUN
      REAL T,Y(2),TOUT,RELERR,ABSERR, TFINAL,TPRINT,WORK(15)
      INTEGER IWORK(5),IFLAG
      INTEGER, PARAMETER :: NEQN = 2
      T=0.0
      Y(1)=3.0
      Y(2)=-1.0
      RELERR=1.0E-4
      ABSERR=0.0
      TFINAL=0.15
      TPRINT=0.0075
      IFLAG=1
      TOUT=T
   10 CALL RKF45(FUN,NEQN,Y,T,TOUT,RELERR,ABSERR,IFLAG,WORK,IWORK)
      PRINT 11,T,Y(1),Y(2)
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
   11 FORMAT(' T=',F10.5,5X,'Y1=',F15.6,5X,'Y2=',F15.6)
   31 FORMAT(' Errors boundaries have changed  '/' RELERR=',E10.3,2X,'ABSERR=',E10.3)
   41 FORMAT(' Many steps ')
   71 FORMAT(' Many outlets ')
   81 FORMAT(' Irregular call ')
      end
