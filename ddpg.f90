subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_network, only: network_type, network_constructor, network_output_single
    use mod_layer, only: layer_set_activation
    
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    real :: state(1), action(1)
    real :: lower_bound, upper_bound
    type(network_type) :: actor_model
    real, allocatable :: sampled_action(:)
    integer :: i
    
    ! Initialize bounds
    lower_bound = -5.0
    upper_bound = 5.0
    
    ! Initialize arrays
    state = 0.0
    action = 0.0
    
    ! Initialize random seed
    call random_seed()
    
    ! Test 1: Try to create network without activation setting
    actor_model = network_constructor([1, 4, 1], activation='relu')
    
    ! Test 2: Skip layer_set_activation for now
    ! call layer_set_activation(actor_model%layers(3), 'tanh')
    
    ! Set state
    state(1) = state_1
    
    ! Test 3: Try simple action without network output
    ! allocate(sampled_action(1))
    ! sampled_action = network_output_single(actor_model, state)
    ! action(1) = sampled_action(1) * upper_bound
    
    ! Use simple action for now
    action(1) = state(1) * 0.1
    
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