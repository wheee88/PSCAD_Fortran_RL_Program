subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    real :: state(1), action(1)
    real :: lower_bound, upper_bound
    real :: noise
    integer :: FID
    
    ! Initialize bounds
    lower_bound = -5.0
    upper_bound = 5.0
    
    ! Initialize arrays
    state = 0.0
    action = 0.0
    
    ! Initialize random seed
    call random_seed()
    
    ! Set state
    state(1) = state_1
    
    ! Simple action with noise
    if (Simu_Step_In == 0) then
        action(1) = 0.0
    else
        ! Simple proportional control with small noise
        call random_number(noise)
        noise = (noise - 0.5) * 0.1  ! Small noise between -0.05 and 0.05
        action(1) = state(1) * 0.1 + noise
    end if
    
    ! Ensure action is within bounds
    if (action(1) < lower_bound) then
        action(1) = lower_bound
    elseif (action(1) > upper_bound) then
        action(1) = upper_bound
    end if
    
    ! Test file I/O (simple version)
    if (Simu_Step_In == 0) then
        Open(Unit=FID, File="test_output.txt", action='write')
        Write(FID, *) "DDPG started"
        Close(FID)
    end if
    
    ! Set outputs
    action_1 = action(1)
    Simu_Step_Out = Simu_Step_In + 1
    
end subroutine ddpg