module basic_controller_fcns
use misc_mod
! Types
type basicdr

integer :: strat , stepno
real*8 :: dr, R
real*8 :: arrayK(50), arraypitch(30)
integer :: ct1 , ct2 , Ksize , Thetasize
real*8 :: Kmin , Kmax , Kopt , Thetamin, Thetamax
real*8 :: errtol , movaverage , timeout , timein

end type basicdr

type(basicdr) basicst



!*****************************************************************************************
    contains
!*****************************************************************************************

function average_error(stepno,sizemoving,val)

real(mk) :: average_error(1)
! ******
real(mk) :: arrayval(1000000)
real(mk) :: arraymoving(1000000)
real(mk) :: val, errval , errout
integer :: stepno, sizemoving
! *** main program ** 
! ** 
if (stepno.lt.2) then
! start parameters with stepno  / arraymoving and arrayval
arraymoving(1:sizemoving) = 200        
arrayval(1:sizemoving) = 0
arrayval(1) =  val
else
! moving average 
!       
! **move average
arrayval(2:sizemoving) = arrayval(1:sizemoving-1)
arrayval(1) = val
! ** move value 
errval = val-arrayval(2)
arraymoving(2:sizemoving) = arraymoving(1:sizemoving)
arraymoving(1) = abs(errval)

! ** error out moving average
errout = sum(arraymoving(1:sizemoving))/sizemoving

endif
! ** out
! **
average_error(1) = errout

end function average_error


    
end module basic_controller_fcns