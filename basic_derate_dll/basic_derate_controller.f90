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
      !    3: constant 3 ; Threshold (deg)./.......
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
      basicst%N = array1(10)
      basicst%timeout = array1(11)    
      basicst%timein = array1(12)  
      ! ** build K and Theta array      
      !
      a = (basicst%Kmax - basicst%Kmin)/(basicst%Ksize - 1)
      arrayK =     (/((basicst%Kmin + i*a), i=0, basicst%Ksize)/)
      !w
      a2 = (basicst%Thetamax - basicst%Thetamin)/(basicst%Thetasize - 1)
      arraypitch = (/((basicst%Thetamin + i2*a2), i2=0, basicst%Thetasize)/)
      ! * save array in structure for update
      basicst%arrayK = arrayK
      basicst%arraypitch = arraypitch
      basicst%stepno = 0 
      
      allocate(basicst%omegas(basicst%N))
      basicst%omegas = 0.0
      basicst%omegas(1)= 1
      ! * initialize counters 
      basicst%ct1 = 1
      basicst%ct2 = 1
      ! * open file and heading 

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
      real(mk) array1(100), array2(100)
      real(mk) :: time , omega , tq_in , tq_out , pitch_in , pitch_out , pe  , aero_thrust, max_error
      real(mk) :: TSR , wsp, CP, CT 
      integer :: ct1, ct2
      logical :: tol_reached, N_ignored, max_time_reached
      
      ! *** get update values in dt *** ! 
      time = array1(1)
      omega = array1(2)
      tq_in = array1(3)
      pitch_in = array1(4)*(180/pi)
      pe = array1(5)
      aero_thrust = array1(6)
      wsp = array1(8)
      ! ***     
      
      ! update array of past N omegas
      basicst%omegas(2:basicst%N) = basicst%omegas(1:basicst%N-1)
      basicst%omegas(1) = omega
      
      basicst%stepno = basicst%stepno + 1 
        
      max_error = max_dev(basicst%omegas, basicst%N)
      ! Determine conditions met      
      tol_reached = (max_error .lt.basicst%errtol)
      N_ignored = (basicst%stepno.gt.basicst%timein)
      max_time_reached = basicst%stepno.gt.basicst%timeout
      
      
      ! if the maximum error is less than the tolerance and N_ignore steps have been ignored, THEN switch
      ! to next K, theta
      if (tol_reached .and. N_ignored) then
        ! write line to file
        TSR = (omega*basicst%R)/wsp
        CP = pe/(0.5*1.225*wsp**3*pi*basicst%R**2)
        CT = (aero_thrust*1000)/(0.5*1.225*wsp**2*pi*basicst%R**2)    
        write(1, '(E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6)') wsp , omega , pitch_in , basicst%arrayK(basicst%ct1), pe , aero_thrust , TSR, CP , CT , max_error
    
            
        ! Increment K, pitch value
        basicst%ct2 = basicst%ct2 + 1 
        basicst%stepno = 0 
        if (basicst%ct2.eq.basicst%Thetasize) then 
            basicst%ct2 = 1
            basicst%ct1 =  basicst%ct1 + 1 
            write(0,*) ' Loop Theta done '
        endif
        
        write(0,*) ' new K value ' ,  basicst%arrayK(basicst%ct1)
        write(0,*) ' new pitch ' ,  basicst%arraypitch(basicst%ct2)    
        
        ! If simulation is finished...        
        if (basicst%ct2.eq.basicst%Ksize) then 
            
            close(1)   
            stop
        endif 
      
        
        elseif (max_time_reached) then 
        ! write line to file
        TSR = (omega*basicst%R)/wsp
        CP = pe/(0.5*1.225*wsp**3*pi*basicst%R**2)
        CT = (aero_thrust*1000)/(0.5*1.225*wsp**2*pi*basicst%R**2)    
        write(1, '(E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6,X,E15.6)') wsp , omega , pitch_in , basicst%arrayK(basicst%ct1), -1 , -1, -1, -1, -1 , max_error
    
            
        ! Increment K, pitch value
        basicst%ct2 = basicst%ct2 + 1 
        basicst%stepno = 0 
        if (basicst%ct2.eq.basicst%Thetasize) then 
            basicst%ct2 = 1
            basicst%ct1 =  basicst%ct1 + 1 
            write(0,*) ' Loop Theta done '
        endif
        
        write(0,*) ' new K value ' ,  basicst%arrayK(basicst%ct1)
        write(0,*) ' new pitch ' ,  basicst%arraypitch(basicst%ct2)    
        
        ! If simulation is finished...        
        if (basicst%ct2.eq.basicst%Ksize) then 
            
            close(1)   
            stop
        endif           
          
      endif
    
        
    !**** Output DLL **** ! 
    ! (1) torque  (2)  pitch 

    array2(1) =   basicst%Kopt*basicst%arrayK(basicst%ct1)*omega**2
    array2(2) =   basicst%arraypitch(basicst%ct2)*(pi/180)
    array2(3) = max_error
   
    
   
    ! **** Output DLL *** !
        
   end subroutine update_controller
!**************************************************************************************************
! *** Interpolate function ***

end module yaw_mod
