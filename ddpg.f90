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
    
    ! Initialize bounds
    lower_bound = -5.0
    upper_bound = 5.0
    
    ! Initialize arrays
    state = 0.0
    action = 0.0
    
    ! Simple policy: just return a constant action for now
    ! This will help us identify if the issue is with the neural network or something else
    state(1) = state_1
    
    ! Simple action selection (no neural network for now)
    if (Simu_Step_In == 0) then
        action(1) = 0.0  ! Start with zero action
    else
        ! Simple proportional control
        action(1) = state(1) * 0.1
    end if
    
    ! Ensure action is within bounds
    if (action(1) < lower_bound) then
        action(1) = lower_bound
    elseif (action(1) > upper_bound) then
        action(1) = upper_bound
    end if
    
    ! Set outputs
    action_1 = action(1)
    Simu_Step_Out = Simu_Step_In + 1
    
end subroutine ddpg