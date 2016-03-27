 SUBROUTINE FUN(T,X,YP)
      REAL T,X(2),YP(2)
      YP(1)=-130*X(1)+900*X(2)+exp(-10*T)
      YP(2)=30*X(1)-300*X(2)+log(1+100*T*T)
      RETURN
      END

program main
  implicit none
  REAL X(2), H, T, TOUT, YP(2), K1(2), K2(2), K3(2)
  T = 0.0
  TOUT = 0.15
  H = 0.0035
  X(1)=3.0
  X(2)=-1.0
  DO WHILE(T < TOUT)
  PRINT 11,T,X(1),X(2)
  CALL FUN(T, X, YP)
  K1 = H * YP
  CALL FUN(T + H/2.0, X + K1/2.0, YP)
  K2 = H * YP
  CALL FUN(T + 3.0*H/4.0, X + 3.0*K2/4.0, YP)
  K3 = H * YP
  X = X + (2.0*K1 + 3.0*K2 + 4.0*K3)/9.0
  T = T + H
  ENDDO

 11 FORMAT(' T=',F10.5,5X,'Y1=',F15.6,5X,'Y2=',F15.6)
end
