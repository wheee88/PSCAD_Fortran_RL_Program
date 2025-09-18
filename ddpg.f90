subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_random, only: randn
    use mod_activation, only: relu, sigmoid, tanhf
    use mod_layer, only: layer_type, layer_constructor, layer_set_activation, layer_activation
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    type(layer_type) :: actor_layer1, actor_layer2
    real :: state(1), action(1)
    real :: lower_bound, upper_bound
    real :: noise(1)
    real :: z1(2), z2(1), output(1)
    real :: h1, h2
    
    ! DDPG parameters
    lower_bound = -5.0
    upper_bound = 5.0
    
    ! Initialize arrays
    state = 0.0
    action = 0.0
    z1 = 0.0
    h1 = 0.0
    h2 = 0.0
    z2 = 0.0
    output = 0.0
    
    ! Initialize random seed
    call random_seed()
    
    ! Set state
    state(1) = state_1
    
    ! Initialize networks (only once)
    if (Simu_Step_In == 0) then
        ! Create simple 2-layer actor network
        actor_layer1 = layer_constructor(1, 2)  ! Input to hidden
        actor_layer2 = layer_constructor(2, 1)  ! Hidden to output
        
        ! Set activation functions
        call layer_set_activation(actor_layer1, 'relu')
        call layer_set_activation(actor_layer2, 'tanh')
        
        action(1) = 0.0
    else
        ! Simple 2-layer actor network forward pass
        if (allocated(actor_layer1 % w) .and. allocated(actor_layer1 % b)) then
            if (size(actor_layer1 % w, 1) >= 1 .and. size(actor_layer1 % w, 2) >= 2 .and. size(actor_layer1 % b) >= 2) then
                ! Layer 1: input -> hidden
                z1(1) = state(1) * actor_layer1 % w(1, 1) + actor_layer1 % b(1)
                z1(2) = state(1) * actor_layer1 % w(1, 2) + actor_layer1 % b(2)
                if (z1(1) > 0.0) hidden(1) = z1(1) else hidden(1) = 0.0  ! ReLU
                if (z1(2) > 0.0) hidden(2) = z1(2) else hidden(2) = 0.0  ! ReLU
                
                ! Layer 2: hidden -> output
                if (allocated(actor_layer2 % w) .and. allocated(actor_layer2 % b)) then
                    if (size(actor_layer2 % w, 1) >= 2 .and. size(actor_layer2 % w, 2) >= 1 .and. size(actor_layer2 % b) >= 1) then
                        z2(1) = hidden(1) * actor_layer2 % w(1, 1) + hidden(2) * actor_layer2 % w(2, 1) + actor_layer2 % b(1)
                        output(1) = tanh(z2(1))  ! Tanh activation
                        action(1) = output(1) * upper_bound  ! Scale to action bounds
                    else
                        action(1) = hidden(1) * 0.1
                    end if
                else
                    action(1) = hidden(1) * 0.1
                end if
            else
                action(1) = state(1) * 0.1
            end if
        else
            action(1) = state(1) * 0.1
        end if
        
        ! Add exploration noise
        noise = randn(1)
        action(1) = action(1) + noise(1) * 0.1
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