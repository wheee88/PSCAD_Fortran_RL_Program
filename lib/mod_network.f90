module mod_network

  use mod_layer, only: array1d, array2d, db_init, dw_init,&
                       db_co_sum, dw_co_sum, layer_type, layer_constructor, &
                       layer_set_activation, layer_activation, layer_activation_prime
  use mod_parallel, only: tile_indices

  implicit none

  private
  public :: network_type, network_constructor, network_init, network_set_activation, &
            network_output_single, network_output_batch, network_save, network_load, &
            network_train_single, network_sync

  type :: network_type
    type(layer_type), allocatable :: layers(:)
    integer, allocatable :: dims(:)
  end type network_type

contains

  type(network_type) function network_constructor(dims, activation) result(net)
    ! Network class constructor. Size of input array dims indicates the total
    ! number of layers (input + hidden + output), and the value of its elements
    ! corresponds the size of each layer.
    integer, intent(in) :: dims(:)
    character(len=*), intent(in), optional :: activation
    call network_init(net, dims)
    if (present(activation)) then
      call network_set_activation(net, activation)
    else
      call network_set_activation(net, 'sigmoid')
    end if
    ! call network_sync(net, 1)  ! Commented out to avoid CAF issues
  end function network_constructor

  subroutine network_init(net, dims)
    ! Allocates and initializes the layers with given dimensions dims.
    type(network_type), intent(in out) :: net
    integer, intent(in) :: dims(:)
    integer :: n
    net % dims = dims
    if (.not. allocated(net % layers)) allocate(net % layers(size(dims)))
    do n = 1, size(dims) - 1
      net % layers(n) = layer_constructor(dims(n), dims(n+1))
    end do
    net % layers(n) = layer_constructor(dims(n), 1)
    net % layers(1) % b = 0
    net % layers(size(dims)) % w = 0
  end subroutine network_init

  subroutine network_set_activation(net, activation)
    ! Sets activation function for all layers
    type(network_type), intent(in out) :: net
    character(len=*), intent(in) :: activation
    integer :: n
    do n = 1, size(net % layers)
      call layer_set_activation(net % layers(n), activation)
    end do
  end subroutine network_set_activation

  function network_output_single(net, x) result(a)
    ! Use forward propagation to compute the output of the network.
    ! This specific procedure is for a single sample of 1-d input data.
    type(network_type), intent(in) :: net
    real, intent(in) :: x(:)
    real, allocatable :: a(:)
    integer :: n
    a = layer_activation(net % layers(2), matmul(transpose(net % layers(1) % w), x) + net % layers(2) % b)
    do n = 3, size(net % layers)
      a = layer_activation(net % layers(n), matmul(transpose(net % layers(n-1) % w), a) + net % layers(n) % b)
    end do
  end function network_output_single

  function network_output_batch(net, x) result(a)
    ! Use forward propagation to compute the output of the network.
    ! This specific procedure is for a batch of 1-d input data.
    type(network_type), intent(in) :: net
    real, intent(in) :: x(:,:)
    real, allocatable :: a(:,:)
    integer :: i
    allocate(a(net % dims(size(net % dims)), size(x, dim=2)))
    do i = 1, size(x, dim=2)
     a(:,i) = network_output_single(net, x(:,i))
    end do
  end function network_output_batch

  subroutine network_save(net, filename)
    ! Saves the network to a file.
    type(network_type), intent(in) :: net
    character(len=*), intent(in) :: filename
    integer :: fileunit, n
    fileunit = 11
    open(unit=fileunit, file=filename)
    write(fileunit, fmt=*) size(net % dims)
    write(fileunit, fmt=*) net % dims
    do n = 1, size(net % dims)
      write(fileunit, fmt=*) n, net % layers(n) % activation_str
    end do
    do n = 2, size(net % dims)
      write(fileunit, fmt=*) net % layers(n) % b
    end do
    do n = 1, size(net % dims) - 1
      write(fileunit, fmt=*) net % layers(n) % w
    end do
    close(fileunit)
  end subroutine network_save

  subroutine network_load(net, filename)
    ! Loads the network from file.
    type(network_type), intent(in out) :: net
    character(len=*), intent(in) :: filename
    integer :: fileunit, n, num_layers, layer_idx
    integer, allocatable :: dims(:)
    character(len=100) :: buffer ! activation string
    fileunit = 10
    open(unit=fileunit, file=filename, status='old', action='read')
    read(fileunit, *) num_layers
    allocate(dims(num_layers))
    read(fileunit, *) dims
    call network_init(net, dims)
    do n = 1, num_layers
      read(fileunit, *) layer_idx, buffer
      call layer_set_activation(net % layers(layer_idx), trim(buffer))
    end do
    do n = 2, size(net % dims)
      read(fileunit, *) net % layers(n) % b
    end do
    do n = 1, size(net % dims) - 1
      read(fileunit, *) net % layers(n) % w
    end do
    close(fileunit)
  end subroutine network_load

  subroutine network_train_single(net, x, y, eta)
    ! Trains a network using a single set of input data x and output data y,
    ! and learning rate eta.
    type(network_type), intent(in out) :: net
    real, intent(in) :: x(:), y(:), eta
    ! Simplified training - just forward pass for now
    ! More complex training would require backpropagation implementation
  end subroutine network_train_single

  subroutine network_sync(net, image)
    ! Broadcasts network weights and biases from
    ! specified image to all others.
    type(network_type), intent(in out) :: net
    integer, intent(in) :: image
    integer :: n
#ifdef CAF
    if (num_images() == 1) return
    do n = 1, size(net % dims)
      call co_broadcast(net % layers(n) % b, image)
      call co_broadcast(net % layers(n) % w, image)
    end do
#else
    ! Single image mode - no synchronization needed
    return
#endif
  end subroutine network_sync

end module mod_network