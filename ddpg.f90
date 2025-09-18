subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_random, only: randn
    use mod_activation, only: relu, sigmoid, tanhf
    use mod_layer, only: layer_type, layer_constructor, layer_set_activation
    implicit none
    
    ! Input/Output parameters
    real, intent(in) :: state_1, reward, Done
    integer, intent(in) :: Simu_Step_In
    real, intent(out) :: action_1
    integer, intent(out) :: Simu_Step_Out
    
    ! Local variables
    type(layer_type) :: layer1, layer2, layer3
    real :: state(1), action(1)
    real :: lower_bound, upper_bound
    real :: noise(1)
    real :: temp_value(1)
    
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
    
    ! Test: Manual network construction without accessing weights/biases
    if (Simu_Step_In == 0) then
        ! Create layers manually
        layer1 = layer_constructor(1, 2)  ! Input to hidden
        layer2 = layer_constructor(2, 1)  ! Hidden to output
        layer3 = layer_constructor(1, 1)  ! Output layer
        
        ! Set activation functions
        call layer_set_activation(layer1, 'relu')
        call layer_set_activation(layer2, 'relu')
        call layer_set_activation(layer3, 'linear')
        
        action(1) = 0.0
    else
        ! Simple action without accessing layer weights/biases
        noise = randn(1)
        temp_value(1) = state(1) * 0.1 + noise(1) * 0.1
        temp_value = relu(temp_value)  ! Apply ReLU activation
        temp_value = sigmoid(temp_value)  ! Apply sigmoid activation
        
        action(1) = temp_value(1) * 2.0 - 1.0  ! Scale to [-1, 1]
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