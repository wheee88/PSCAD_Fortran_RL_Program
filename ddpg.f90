subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_network, only: network_type, network_constructor
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    type(network_type) :: actor_model
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
    
    ! Test 1: Just create network (no output, no activation setting)
    if (Simu_Step_In == 0) then
        ! Create a very simple network
        actor_model = network_constructor([1, 2, 1], activation='linear')
        
        ! Test file I/O
        Open(Unit=FID, File="test_network.txt", action='write')
        Write(FID, *) "Network created successfully"
        Close(FID)
    end if
    
    ! Simple action without using network
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
    
    ! Set outputs
    action_1 = action(1)
    Simu_Step_Out = Simu_Step_In + 1
    
end subroutine ddpg