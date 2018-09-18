module yaw_mod
   use basic_controller_fcns
   
   contains
!**************************************************************************************************
   subroutine init_controller(array1,array2)
!      use write_version_mod
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'init_controller'::init_controller
      !DEC$ END IF
      real(mk) array1(100), array2(1), arrayK(50), arraypitch(30)
      integer :: i , i2
      real*8 :: a , a2
      character(len=1020) :: filename
      
      !real(mk) ,dimension(yawst%larray) :: arrayaux
      ! Input array1 must contain
      !    1: constant 1 ; Time Start 
      !    2: constant 2 ; Array distance (mean deficit /dt)
      !    3: constant 3 ; Threshold (deg)
      write(0,*) 'CP-TSR curve controller access..'
      ! * get R turbine
      basicst%R = array1(1)
      ! * get K parameters 
      basicst%Kopt = array1(2)
      basicst%Kmin = array1(3)
      basicst%Kmax = array1(4)
      basicst%Ksize = array1(5)
      ! * get pitch parameters      
      basicst%Thetamin = array1(6)
      basicst%Thetamax = array1(7)
      basicst%Thetasize = array1(8)
      ! * convergence criteria 
      basicst%errtol = array1(9)
      basicst%movaverage = array1(10)
      basicst%timeout = array1(11)    
      basicst%timein = array1(12)  
      ! ** build K and Theta array      
      !
      a = (basicst%Kmax-basicst%Kmin)/(basicst%Ksize-1)
      arrayK =     (/((basicst%Kmin + i*a),i=0, basicst%Ksize)/)
      !w
      a2 = (basicst%Thetamax-basicst%Thetamin)/(basicst%Thetasize-1)
      arraypitch = (/((basicst%Thetamin + i2*a2),i2=0, basicst%Thetasize)/)
      ! * save array in structure for update
      basicst%arrayK = arrayK
      basicst%arraypitch = arraypitch
      basicst%stepno = 0 
      ! * initialize counters 
      basicst%ct1 = 1
      basicst%ct2 = 1
      ! * open file and heading 
      
      !write(filename, "(A13)")  "steady_states"
      
      open(unit = 1, file = 'steady_states.txt')
      write(1,*) "Steady states values for CP-TSR Curve" 
      write(1,*) "Format : wsp , omega , pitch_in ,K , pe , aero_thrust , TSR, CP , CT " 
      
   end subroutine init_controller
!**************************************************************************************************
!**************************************************************************************************
!**************************************************************************************************
   subroutine update_controller(array1, array2)
      implicit none
      !DEC$ IF .NOT. DEFINED(__LINUX__)
      !DEC$ ATTRIBUTES DLLEXPORT, C, ALIAS:'update_controller'::update_controller
      !DEC$ END IF
      real(mk) :: time , omega , tq_in , tq_out , pitch_in , pitch_out , pe  , aero_thrust
      real(mk) array1(100), array2(100), arrayaux(10000000)
      integer :: i , movav
      real(mk) :: err1(1), TSR , wsp
      real(mk) :: CP, CT 
      integer :: ct1,ct2 , selectc , flagq
      
      ! *** get update values in dt *** ! 
      time = array1(1)
      omega = array1(2)
      tq_in = array1(3)
      pitch_in = array1(4)*(180/pi)
      pe = array1(5)
      aero_thrust = array1(6)
      wsp = array1(8)
      ! ***     
      basicst%stepno = basicst%stepno + 1 
      
      movav = basicst%movaverage
      err1 = average_error(basicst%stepno,movav,omega)
      !write(0,*) 'err1' , err1
      ! ***     
      
      
      
      if ((err1(1).lt.basicst%errtol).and.(basicst%stepno.gt.basicst%timein)) then
          
          selectc = 1 
          flagq = 1
          
      elseif (basicst%stepno.gt.basicst%timeout) then 
          
          selectc = 1 
          flagq = 0 
          
      
      else
          selectc = 0
      endif
      
      
      
      
      select case (selectc)
          
      case(1) 
          
          
        TSR = (omega*basicst%R)/wsp
        CP = pe/(0.5*1.225*wsp**3*pi*basicst%R**2)
        CT = (aero_thrust*1000)/(0.5*1.225*wsp**2*pi*basicst%R**2)
        
        write(1, '(E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6)') wsp , omega , pitch_in , basicst%arrayK(basicst%ct1), pe , aero_thrust , TSR, CP , CT , err1
        
        basicst%ct2 =  basicst%ct2 +1 
        basicst%stepno = 0 
        
        write(0,*) 'ct1 ' ,  basicst%ct1
        write(0,*) 'ct2 ' ,  basicst%ct2
        write(0,*) ' new K value ' ,  basicst%arrayK(basicst%ct1)
        write(0,*) ' new pitch ' ,  basicst%arraypitch(basicst%ct2)    
        
        if (basicst%ct2.eq.basicst%Thetasize) then 
            basicst%ct2 = 1
            basicst%ct1 =  basicst%ct1 + 1 
            write(0,*) ' Loop Theta done '
        endif
              
        if (basicst%ct2.eq.basicst%Ksize) then 
            
            close(1)   
            stop
            
        endif
          
      end select 
          
      
        
     
        
        
        
        
   
        
    !**** Output DLL **** ! 
    ! (1) torque  (2)  pitch 

    array2(1) =   basicst%Kopt*basicst%arrayK(basicst%ct1)*omega**2
    array2(2) =   basicst%arraypitch(basicst%ct2)*(pi/180)
   
    
   
    ! **** Output DLL *** !
        
   end subroutine update_controller
!**************************************************************************************************
! *** Interpolate function ***

end module yaw_mod
