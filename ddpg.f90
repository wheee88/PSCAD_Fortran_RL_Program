subroutine ddpg(state_1,reward,Done,Simu_Step_In,action_1,Simu_Step_Out)
    use mod_OUActionNoise, only: noise_type
    use mod_network, only: network_type
    use mod_buffer, only: buffer_type
    
    implicit none
    type(buffer_type) :: buffer
    integer :: buffer_capacity, batch_size, num_states, num_actions, Simu_Step_In, Simu_Step_Out
    real :: state_1, action_1
    real :: state(1), action(1)
    real :: prev_state(1)
    real :: reward, Done, episodic_reward
    real :: episodic_reward_store(100)
    real :: critic_lr, actor_lr, gamma, tau
    type(network_type) :: actor_model, critic_model_1, critic_model_2, critic_model, target_actor, target_critic_1, target_critic_2, target_critic
    type(noise_type) :: ou_noise
    real :: mean, std_dev, lower_bound, upper_bound
    logical :: Train, alive
    integer :: FID, i, episode_counter
    
    !! Environment setting
    num_states =  1
    num_actions = 1

    upper_bound = 5
    lower_bound = -5
    
    !! Training hyperparameters
    mean = 0
    std_dev = 0.2
    ou_noise = noise_type(mean, std_dev)
     
    actor_model = network_type([num_states, 256, 200, num_actions], activation='relu') !!!这里有个小bug，相邻两层的layer的neuron之积不能太大
    call actor_model%layers(4)%set_activation('tanh')
    critic_model_1 = network_type([num_states, 16, 32], activation='relu')
    critic_model_2 = network_type([num_actions, 32], activation='relu')
    critic_model = network_type([64,256,200,1], activation='relu')
    call critic_model%layers(4)%set_activation('linear')
    
    target_actor = network_type([num_states, 256, 200, num_actions], activation='relu')
    call target_actor%layers(4)%set_activation('tanh')
    target_critic_1 = network_type([num_states, 16, 32], activation='relu')
    target_critic_2 = network_type([num_actions, 32], activation='relu')
    target_critic = network_type([64,256,200,1], activation='relu')
    call target_critic%layers(4)%set_activation('linear')
    
    ! Making the weights equal initially
    do i = 1, size(actor_model%layers)
        target_actor%layers(i)%b = actor_model%layers(i)%b
        target_actor%layers(i)%w = actor_model%layers(i)%w
    end do
    do i = 1, size(critic_model_1%layers)
        target_critic_1%layers(i)%b = critic_model_1%layers(i)%b
        target_critic_1%layers(i)%w = critic_model_1%layers(i)%w
    end do
    do i = 1, size(critic_model_2%layers)
        target_critic_2%layers(i)%b = critic_model_2%layers(i)%b
        target_critic_2%layers(i)%w = critic_model_2%layers(i)%w
    end do   
    do i = 1, size(critic_model%layers)
        target_critic%layers(i)%b = critic_model%layers(i)%b
        target_critic%layers(i)%w = critic_model%layers(i)%w
    end do
    
    ! Learning rate for actor-critic models
    critic_lr = 0.0002
    actor_lr = 0.0001

    ! Discount factor for future rewards
    gamma = 0.9
    ! Used to update target networks
    tau = 0.0005

    ! Create experience buffer
    buffer_capacity = 50000
    batch_size = 64
    buffer = buffer_type(buffer_capacity, batch_size, num_states, num_actions)
    
    episodic_reward = 0
    ! Train or not Train
    Train = .true.
    
    !! Start Training
    if (Train) then 
        if (Simu_Step_In == 0) then
            INQUIRE (file='D:\PSCAD_RL_Test\Fortran_DDPG\PSCAD_DDPG.if15\PSCAD_actor.txt', exist=alive)
            if (alive) then
                !/// Not the fist eisode: Load the weights
                call actor_model%load("PSCAD_actor.txt")
                call critic_model_1%load("PSCAD_critic_1.txt")
                call critic_model_2%load("PSCAD_critic_2.txt")
                call critic_model%load("PSCAD_critic.txt")
                call target_actor%load("PSCAD_target_actor.txt")
                call target_critic_1%load("PSCAD_target_critic_1.txt")
                call target_critic_2%load("PSCAD_target_critic_2.txt")
                call target_critic%load("PSCAD_target_critic.txt")
            else
                !/// First epiosde: Save the weights
                call actor_model%save("PSCAD_actor.txt")
                call critic_model_1%save("PSCAD_critic_1.txt")
                call critic_model_2%save("PSCAD_critic_2.txt")
                call critic_model%save("PSCAD_critic.txt")
                call target_actor%save("PSCAD_target_actor.txt")
                call target_critic_1%save("PSCAD_target_critic_1.txt")
                call target_critic_2%save("PSCAD_target_critic_2.txt")
                call target_critic%save("PSCAD_target_critic.txt")
            
                ! Save the experience buffer
                Open(NewUnit=FID, File="buffer_counter_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) buffer%buffer_counter
                Close(FID)
                Open(NewUnit=FID, File="state_buffer_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) buffer%state_buffer
                Close(FID)
                Open(NewUnit=FID, File="action_buffer_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) buffer%action_buffer
                Close(FID)
                Open(NewUnit=FID, File="reward_buffer_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) buffer%reward_buffer
                Close(FID)
                Open(NewUnit=FID, File="next_state_buffer_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) buffer%next_state_buffer
                Close(FID)
            
                ! Save the episode reward for final plotting
                episode_counter = 1
                Open(NewUnit=FID, File="episode_counter",action='readwrite',form='unformatted',access='stream')
                Write(FID) episode_counter
                Close(FID)
                episodic_reward_store(episode_counter) = 0
                Open(NewUnit=FID, File="episodic_reward_store",action='readwrite',form='unformatted',access='stream')
                Write(FID) episodic_reward_store
                Close(FID)

            end if
            
            !/// First step in each episode: store the 'episodic_reward' and 'prev_state'
            Open(NewUnit=FID, File="episodic_reward",action='readwrite',form='unformatted',access='stream')
            Write(FID) episodic_reward
            Close(FID)
            
            ! iterate the state to prev_state
            state(1) = state_1
            prev_state = state
            Open(NewUnit=FID, File="prev_state",action='readwrite',form='unformatted',access='stream')
            Write(FID) prev_state
            Close(FID)
            
            ! Excute action and save it
            action = policy(state, lower_bound, upper_bound, actor_model, ou_noise)        
            Open(NewUnit=FID, File="action",action='readwrite',form='unformatted',access='stream')
            Write(FID) action
            Close(FID)
            action_1 = action(1)
            Simu_Step_Out = Simu_Step_In + 1  
        else 
            ! Load previous state and previous action
            Open(NewUnit=FID, File="prev_state",action='readwrite',form='unformatted',access='stream')
            Read(FID) prev_state
            !print *, prev_state
            Close(FID)
            Open(NewUnit=FID, File="action",action='readwrite',form='unformatted',access='stream')
            Read(FID) action
            Close(FID)
            
            ! Load the weights
            call actor_model%load("PSCAD_actor.txt")
            call critic_model_1%load("PSCAD_critic_1.txt")
            call critic_model_2%load("PSCAD_critic_2.txt")
            call critic_model%load("PSCAD_critic.txt")
            call target_actor%load("PSCAD_target_actor.txt")
            call target_critic_1%load("PSCAD_target_critic_1.txt")
            call target_critic_2%load("PSCAD_target_critic_2.txt")
            call target_critic%load("PSCAD_target_critic.txt")
            
            ! Load the experience buffer
            Open(NewUnit=FID, File="buffer_counter_store",action='readwrite',form='unformatted',access='stream')
            Read(FID) buffer%buffer_counter
            !print *, buffer%buffer_counter
            Close(FID)
            Open(NewUnit=FID, File="state_buffer_store",action='readwrite',form='unformatted',access='stream')
            Read(FID) buffer%state_buffer

            Close(FID)
            Open(NewUnit=FID, File="action_buffer_store",action='readwrite',form='unformatted',access='stream')
            Read(FID) buffer%action_buffer
            Close(FID)
            Open(NewUnit=FID, File="reward_buffer_store",action='readwrite',form='unformatted',access='stream')
            Read(FID) buffer%reward_buffer
            Close(FID)
            Open(NewUnit=FID, File="next_state_buffer_store",action='readwrite',form='unformatted',access='stream')
            Read(FID) buffer%next_state_buffer
            Close(FID)
            
            call buffer%nrecord(prev_state, action, reward, state)
            
            Open(NewUnit=FID, File="episodic_reward",action='readwrite',form='unformatted',access='stream')
            Read(FID) episodic_reward
            episodic_reward = episodic_reward + reward
            Write(FID) episodic_reward
            Close(FID)
            
            call buffer%learn(actor_model, critic_model_1, critic_model_2, critic_model, target_actor, target_critic_1, target_critic_2, target_critic, critic_lr, actor_lr, gamma)
            
            ! Update_target network
            do i = 1, size(actor_model%layers)
                target_actor%layers(i)%b = actor_model%layers(i)%b * tau + target_actor%layers(i)%b * (1-tau)
                target_actor%layers(i)%w = actor_model%layers(i)%w * tau + target_actor%layers(i)%w * (1-tau)
            end do
            do i = 1, size(critic_model_1%layers)        
                target_critic_1%layers(i)%b = critic_model_1%layers(i)%b * tau + target_critic_1%layers(i)%b * (1-tau)
                target_critic_1%layers(i)%w = critic_model_1%layers(i)%w * tau + target_critic_1%layers(i)%w * (1-tau)
            end do
            do i = 1, size(critic_model_2%layers)        
                target_critic_2%layers(i)%b = critic_model_2%layers(i)%b * tau + target_critic_2%layers(i)%b * (1-tau)
                target_critic_2%layers(i)%w = critic_model_2%layers(i)%w * tau + target_critic_2%layers(i)%w * (1-tau)
            end do
            do i = 1, size(critic_model%layers)        
                target_critic%layers(i)%b = critic_model%layers(i)%b * tau + target_critic%layers(i)%b * (1-tau)
                target_critic%layers(i)%w = critic_model%layers(i)%w * tau + target_critic%layers(i)%w * (1-tau)
            end do
            
            ! Save the experience buffer
            Open(NewUnit=FID, File="buffer_counter_store",action='readwrite',form='unformatted',access='stream')
            Write(FID) buffer%buffer_counter
            Close(FID)
            Open(NewUnit=FID, File="state_buffer_store",action='readwrite',form='unformatted',access='stream')
            Write(FID) buffer%state_buffer
            Close(FID)
            Open(NewUnit=FID, File="action_buffer_store",action='readwrite',form='unformatted',access='stream')
            Write(FID) buffer%action_buffer
            Close(FID)
            Open(NewUnit=FID, File="reward_buffer_store",action='readwrite',form='unformatted',access='stream')
            Write(FID) buffer%reward_buffer
            Close(FID)
            Open(NewUnit=FID, File="next_state_buffer_store",action='readwrite',form='unformatted',access='stream')
            Write(FID) buffer%next_state_buffer
            Close(FID)

            ! Save the weights
            call actor_model%save("PSCAD_actor.txt")
            call critic_model_1%save("PSCAD_critic_1.txt")
            call critic_model_2%save("PSCAD_critic_2.txt")
            call critic_model%save("PSCAD_critic.txt")
            call target_actor%save("PSCAD_target_actor.txt")
            call target_critic_1%save("PSCAD_target_critic_1.txt")
            call target_critic_2%save("PSCAD_target_critic_2.txt")
            call target_critic%save("PSCAD_target_critic.txt")
            
            ! Excute action and save it
            action = policy(state, lower_bound, upper_bound, actor_model, ou_noise)
            Open(NewUnit=FID, File="action",action='readwrite',form='unformatted',access='stream')
            Write(FID) action
            Close(FID)
            
            ! iterate the state to prev_state
            state(1) = state_1
            prev_state = state
            Open(NewUnit=FID, File="prev_state",action='readwrite',form='unformatted',access='stream')
            Write(FID) prev_state
            Close(FID)
            
            ! Last step of this episode: Save the episode reward to 'episodic_reward_store'
            if (Simu_Step_In == 500) then
                Open(NewUnit=FID, File="episode_counter",action='readwrite',form='unformatted',access='stream')
                Read(FID) episode_counter
                Close(FID)
                Open(NewUnit=FID, File="episodic_reward_store",action='readwrite',form='unformatted',access='stream')
                Read(FID) episodic_reward_store
                episodic_reward_store(episode_counter) = episodic_reward
                Write(FID) episodic_reward_store
                Close(FID)
                episode_counter = episode_counter + 1
                Open(NewUnit=FID, File="episode_counter",action='readwrite',form='unformatted',access='stream')
                Write(FID) episode_counter
                Close(FID)
            end if
            action_1 = action(1)
            Simu_Step_Out = Simu_Step_In + 1
        end if
    !! Not train, just run
    else
        state(1) = state_1
        ! Load the weights
        call actor_model%load("PSCAD_actor.txt")

        ! Excute action
        action = policy(state, lower_bound, upper_bound, actor_model, ou_noise)
        
        action_1 = action(1)
        Simu_Step_Out = Simu_Step_In + 1
    end if

contains
    ! Define policy function for taking action
    function policy(state, lower_bound, upper_bound, actor_model, ou_noise) result(legal_action)
        implicit none
        real, intent(in) :: state(:)
        real, intent(in) :: lower_bound, upper_bound
        type(network_type), intent(in) :: actor_model
        type(noise_type) :: ou_noise
        real, allocatable :: sampled_action(:), legal_action(:)
        real :: noise
        
        allocate(sampled_action(actor_model%dims(size(actor_model%dims))))
        allocate(legal_action(actor_model%dims(size(actor_model%dims))))
        
        sampled_action = actor_model%output(state)
        sampled_action = sampled_action * upper_bound
        noise = ou_noise%ncall()
        ! Adding noise to action
        sampled_action = sampled_action + noise
        
        ! We make sure action is within bounds 
        if (sampled_action(1) < lower_bound) then
            legal_action = lower_bound
        elseif (sampled_action(1) > upper_bound) then
            legal_action = upper_bound
        else
            legal_action = sampled_action
        end if   
    end function policy
    
end subroutine ddpg