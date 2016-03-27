REAl function build (x,x1,arr)
integer x1
REAl, dimension (0:10, 1:2)::arr
REAl num,denom,x
integer i
num = 1
denom = 1
do i=0,10,1
 if ( i /= x1 ) then
 num=(x-arr(i,1))*num
 denom=(arr(x1,1)-arr(i,1))*denom
 endif
enddo
build=arr(x1,2)*num/denom
end function build

REAl function FUN (x)
REAl, dimension (0:10, 1:2)::a
REAl, dimension (0:10)::a1
REAl x, build
integer i
do i = 0,10,1
        a(i,1) = 0.3*i
        a(i,2) = 1-exp(-a(i,1))
 enddo
 do i = 0,10,1
    a1(i) = 1*build(x,i,a)
 enddo
 FUN = sum(a1)
 PRINT *, FUN
end function FUN

program lagrange
 implicit none
 EXTERNAL FUN
 REAl FUN,A,B,RELERR,ABSERR,RESULT,ERREST,FLAG
 INTEGER NOFUN
 A = 0
 B = 3
 RELERR = 1.E-07
 ABSERR = 0
 CALL QUANC8(FUN,A,B,ABSERR,RELERR,RESULT,ERREST,NOFUN,FLAG)
 PRINT *, RESULT, ERREST, NOFUN, FLAG
 end
