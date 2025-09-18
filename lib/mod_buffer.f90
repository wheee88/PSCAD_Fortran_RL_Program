module mod_buffer
    use mod_random, only: randn
    use mod_network, only: network_type, network_output_single, network_output_batch, network_train_single
    implicit none
    
    private
    public :: buffer_type, buffer_constructor, buffer_init, buffer_nrecord, buffer_update, buffer_learn
    
    type :: buffer_type
        integer :: buffer_capacity, batch_size, buffer_counter
        real, allocatable :: state_buffer(:,:), action_buffer(:,:), reward_buffer(:), next_state_buffer(:,:)
    end type buffer_type

contains
    
    type(buffer_type) function buffer_constructor(buffer_capacity, batch_size, num_states, num_actions) result(buffer)
        implicit none
        integer, intent(in) :: buffer_capacity, batch_size, num_states, num_actions
        call buffer_init(buffer, buffer_capacity, batch_size, num_states, num_actions)
    end function buffer_constructor
    
    subroutine buffer_init(self, buffer_capacity, batch_size, num_states, num_actions)
        implicit none
        type(buffer_type), intent(in out) :: self
        integer, intent(in) :: buffer_capacity, batch_size, num_states, num_actions
        allocate(self%state_buffer(num_states,0:buffer_capacity-1))
        allocate(self%action_buffer(num_actions,0:buffer_capacity-1))
        allocate(self%reward_buffer(0:buffer_capacity-1))
        allocate(self%next_state_buffer(num_states,0:buffer_capacity-1))
        ! Number of "experiences" to store at max
        self%buffer_capacity = buffer_capacity
        ! Num of tuples to train on.
        self%batch_size = batch_size
        
        ! Its tells us num of times record() was called.
        self%buffer_counter = 0
        
        ! Instead of list of tuples as the exp.replay concept go
        ! We use different structs for each tuple element
        self%state_buffer = 0   
        self%action_buffer = 0
        self%reward_buffer = 0
        self%next_state_buffer = 0
    end subroutine buffer_init
    
    ! Takes (s,a,r,s') column obervation tuple as input
    subroutine buffer_nrecord(self, prev_state, action, reward, state)
        implicit none
        type(buffer_type), intent(in out) :: self
        integer :: index
        real, intent(in) :: prev_state(:), state(:), action(:)
        real, intent(in) :: reward
        
        ! Set index to zero if buffer_capacity is exceeded,
        ! replacing old records
        index = mod(self%buffer_counter, self%buffer_capacity)

        self%state_buffer(:,index) = prev_state
        self%action_buffer(:,index) = action
        self%reward_buffer(index) = reward
        self%next_state_buffer(:,index) = state

        self%buffer_counter = self%buffer_counter + 1
    end subroutine buffer_nrecord
    
    ! Eager execution is turned on by default in TensorFlow 2. Decorating with tf.function allows
    ! TensorFlow to build a static graph out of the logic and computations in our function.
    ! This provides a large speed up for blocks of code that contain many small TensorFlow operations such as this one.
    !@tf.function
    subroutine buffer_update(self, state_batch, action_batch, reward_batch, next_state_batch, &
                            actor_model, critic_model_1, critic_model_2, critic_model, &
                            target_actor, target_critic_1, target_critic_2, target_critic, &
                            critic_lr, actor_lr, gamma)
        ! Training and updating Actor & Critic networks.
        ! See Pseudo Code.
        implicit none
        type(buffer_type), intent(in out) :: self
        type(network_type) :: actor_model, critic_model_1, critic_model_2, critic_model
        type(network_type) :: target_actor, target_critic_1, target_critic_2, target_critic
        real, intent(in) :: state_batch(:,:), action_batch(:,:), next_state_batch(:,:)
        real, intent(in) :: reward_batch(:)
        real, intent(in) :: critic_lr, actor_lr, gamma
        real, allocatable :: target_actions(:,:), critic_value(:,:), actions(:,:)
        real, allocatable :: critic_value_1(:,:), critic_value_2(:,:)
        real, allocatable :: y1(:,:), y2(:,:)
        real, allocatable :: y12(:,:), critic_value_12(:,:)
        real, allocatable :: y(:,:)
        real, allocatable :: reward_batch_matrix(:,:)
        integer :: i
        
        ! Allocate arrays
        allocate(target_actions(size(action_batch, dim=1), size(action_batch, dim=2)))
        allocate(critic_value(1, size(action_batch, dim=2)))
        allocate(actions(size(action_batch, dim=1), size(action_batch, dim=2)))
        allocate(critic_value_1(32, size(action_batch, dim=2)))
        allocate(critic_value_2(32, size(action_batch, dim=2)))
        allocate(y1(32, size(reward_batch)))
        allocate(y2(32, size(reward_batch)))
        allocate(y12(64, size(reward_batch)))
        allocate(critic_value_12(64, size(action_batch, dim=2)))
        allocate(y(1, size(reward_batch)))
        allocate(reward_batch_matrix(1, size(reward_batch)))
        
        reward_batch_matrix(1,:) = reward_batch(:)
        
        target_actions = network_output_batch(target_actor, next_state_batch) !!
        y1 = network_output_batch(target_critic_1, next_state_batch)
        y2 = network_output_batch(target_critic_2, target_actions)
        !!�˴���Ҫ�������ݽṹ,��ΰ���������ƴ��,(Fortranò�Ʋ������ܾ���ƴ��,�����ֶ�ƴ��)
        do i = 1, size(reward_batch)
            y12(:,i) = [y1(:,i), y2(:,i)]
        end do
        !print *, shape(y12)
        y = reward_batch_matrix + gamma * network_output_batch(target_critic, y12)
        !print *, y(1,1)
        critic_value_1 = network_output_batch(critic_model_1, state_batch)
        critic_value_2 = network_output_batch(critic_model_2, action_batch)
        do i = 1, size(action_batch, dim=2)
            critic_value_12(:,i) = [critic_value_1(:,i), critic_value_2(:,i)]
        end do
        critic_value = network_output_batch(critic_model, critic_value_12)
        ! TODO: Implement batch training
        ! call network_train_single(critic_model_1, state_batch, y1, critic_lr)
        !print *, state_batch(1,1:2)
        !print *, reward_batch(1:2)
        ! call network_train_single(critic_model_2, action_batch, y2, critic_lr)
        ! call network_train_single(critic_model, critic_value_12, y, critic_lr)
        !print *, critic_value_12(1,1) --��Ҫ���Ϊʲô���仯
        !print *, critic_model%layers(1)%w(1,1) --��Ҫ���Ϊʲô���仯
        actions = network_output_batch(actor_model, state_batch)
        
        !y(1,:) = 10
        
        critic_value_1 = network_output_batch(critic_model_1, state_batch)
        critic_value_2 = network_output_batch(critic_model_2, actions)
        do i = 1, size(action_batch, dim=2)
            critic_value_12(:,i) = [critic_value_1(:,i), critic_value_2(:,i)]
        end do
        critic_value = network_output_batch(critic_model, critic_value_12)
        ! Used `-value` as we want to maximize the value given
        ! by the critic for our actions
        !call actor_model%train(critic_value, 0, eta)
        !actor_loss = -(sum(critic_value) / size(critic_value)) !!��һ��������Ҫ�Լ���д���򴫲��㷨
        !print *, actor_model%loss(critic_value(:,1), y(:,1))
        !print *, -critic_value(:,1)
        
        ! TODO: Implement batch training
        ! call network_train_single(actor_model, state_batch, y, actor_lr)
        !call actor_model%train_maximize_batch(state_batch, actor_lr)
        
        ! Deallocate arrays
        deallocate(target_actions, critic_value, actions, critic_value_1, critic_value_2, &
                  y1, y2, y12, critic_value_12, y, reward_batch_matrix)
    end subroutine buffer_update
    
    ! We compute the loss and update parameters
    subroutine buffer_learn(self, actor_model, critic_model_1, critic_model_2, critic_model, &
                           target_actor, target_critic_1, target_critic_2, target_critic, &
                           critic_lr, actor_lr, gamma)
        implicit none
        type(buffer_type), intent(in out) :: self
        real, intent(in) :: critic_lr, actor_lr, gamma
        type(network_type) :: actor_model, critic_model_1, critic_model_2, critic_model
        type(network_type) :: target_actor, target_critic_1, target_critic_2, target_critic
        real, allocatable :: k(:)
        real, allocatable :: state_batch(:,:), action_batch(:,:)
        real, allocatable :: reward_batch(:), next_state_batch(:,:)
        integer, allocatable :: batch_indices(:)
        integer :: record_range
        
        ! Allocate arrays
        allocate(k(self%batch_size))
        allocate(state_batch(size(self%state_buffer, dim=1), self%batch_size))
        allocate(action_batch(size(self%action_buffer, dim=1), self%batch_size))
        allocate(reward_batch(self%batch_size))
        allocate(next_state_batch(size(self%next_state_buffer, dim=1), self%batch_size))
        allocate(batch_indices(self%batch_size))
        
        ! Get sampling range
        record_range = min(self%buffer_counter, self%buffer_capacity)
        ! Randomly sample indices
        !call random_number(record_range, self%batch_size)
        call random_seed()
        call random_number(k)
        k = k * record_range
        batch_indices = floor(k)

        ! Pick corresponding experiences
        state_batch = self%state_buffer(:,batch_indices)
        action_batch = self%action_buffer(:,batch_indices)
        reward_batch = self%reward_buffer(batch_indices)
        next_state_batch = self%next_state_buffer(:,batch_indices)

        call buffer_update(self, state_batch, action_batch, reward_batch, next_state_batch, &
                          actor_model, critic_model_1, critic_model_2, critic_model, &
                          target_actor, target_critic_1, target_critic_2, target_critic, &
                          critic_lr, actor_lr, gamma)
        
        ! Deallocate arrays
        deallocate(k, state_batch, action_batch, reward_batch, next_state_batch, batch_indices)
    end subroutine buffer_learn
end module mod_buffer