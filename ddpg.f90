subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_random, only: randn
    use mod_activation, only: relu, sigmoid, tanh_activation
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    real :: state(1), action(1)
    real :: lower_bound, upper_bound
    real :: noise(1)
    real :: temp_value
    
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
    
    ! Test: Use mod_activation module
    if (Simu_Step_In == 0) then
        action(1) = 0.0
    else
        ! Use randn from mod_random
        noise = randn(1)
        
        ! Test activation functions
        temp_value = state(1) * 0.1 + noise(1) * 0.1
        temp_value = relu(temp_value)  ! Apply ReLU activation
        temp_value = sigmoid(temp_value)  ! Apply sigmoid activation
        
        action(1) = temp_value * 2.0 - 1.0  ! Scale to [-1, 1]
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