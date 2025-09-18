module mod_layer

  ! Defines the layer type and its methods.

  use mod_activation
  use mod_random, only: randn

  implicit none

  private
  public :: array1d, array2d, db_init, db_co_sum, dw_init, dw_co_sum, layer_type
  public :: layer_constructor, layer_set_activation, layer_activation, layer_activation_prime

  type :: layer_type
    real, allocatable :: a(:) ! activations
    real, allocatable :: b(:) ! biases
    real, allocatable :: w(:,:) ! weights
    real, allocatable :: z(:) ! arg. to activation function
    character(len=20) :: activation_str ! activation character string
    integer :: activation_type ! 1=linear, 2=gaussian, 3=relu, 4=sigmoid, 5=step, 6=tanh
  end type layer_type

  type :: array1d
    real, allocatable :: array(:)
  end type array1d

  type :: array2d
    real, allocatable :: array(:,:)
  end type array2d

contains

  type(layer_type) function layer_constructor(this_size, next_size) result(layer)
    ! Layer class constructor. this_size is the number of neurons in the layer.
    ! next_size is the number of neurons in the next layer, used to allocate
    ! the weights.
    integer, intent(in) :: this_size, next_size
    allocate(layer % a(this_size))
    allocate(layer % z(this_size))
    allocate(layer % w(this_size,next_size))
    allocate(layer % b(this_size))
    layer % a = 0
    layer % z = 0
    layer % w = randn(this_size, next_size) / this_size
    layer % b = randn(this_size)
    layer % activation_str = 'sigmoid'
    layer % activation_type = 4
  end function layer_constructor

  pure type(array1d) function array1d_constructor(length) result(a)
    ! Overloads the default type constructor.
    integer, intent(in) :: length
    allocate(a % array(length))
    a % array = 0
  end function array1d_constructor

  pure type(array2d) function array2d_constructor(dims) result(a)
    ! Overloads the default type constructor.
    integer, intent(in) :: dims(2)
    allocate(a % array(dims(1), dims(2)))
    a % array = 0
  end function array2d_constructor

  pure subroutine db_init(db, dims)
    ! Initialises biases structure.
    type(array1d), allocatable, intent(in out) :: db(:)
    integer, intent(in) :: dims(:)
    integer :: n, nm
    nm = size(dims)
    allocate(db(nm))
    do n = 1, nm - 1
      db(n) = array1d_constructor(dims(n))
    end do
    db(n) = array1d_constructor(dims(n))
  end subroutine db_init

  pure subroutine dw_init(dw, dims)
    ! Initialises weights structure.
    type(array2d), allocatable, intent(in out) :: dw(:)
    integer, intent(in) :: dims(:)
    integer :: n, nm
    integer :: dims_temp(2)
    nm = size(dims)
    allocate(dw(nm))
    do n = 1, nm - 1
      dims_temp(1) = dims(n)
      dims_temp(2) = dims(n+1)
      dw(n) = array2d_constructor(dims_temp)
    end do
    dims_temp(1) = dims(n)
    dims_temp(2) = 1
    dw(n) = array2d_constructor(dims_temp)
  end subroutine dw_init

  subroutine db_co_sum(db)
    ! Performs a collective sum of bias tendencies.
    type(array1d), allocatable, intent(in out) :: db(:)
    integer :: n
    do n = 2, size(db)
#ifdef CAF
      call co_sum(db(n) % array)
#endif
    end do
  end subroutine db_co_sum

  subroutine dw_co_sum(dw)
    ! Performs a collective sum of weights tendencies.
    type(array2d), allocatable, intent(in out) :: dw(:)
    integer :: n
    do n = 1, size(dw) - 1
#ifdef CAF
      call co_sum(dw(n) % array)
#endif
    end do
  end subroutine dw_co_sum

  pure elemental subroutine layer_set_activation(layer, activation)
    ! Sets the activation function. Input string must match one of
    ! provided activation functions, otherwise it defaults to sigmoid.
    ! If activation not present, defaults to sigmoid.
    type(layer_type), intent(in out) :: layer
    character(len=*), intent(in) :: activation
    if (trim(activation) == 'linear') then
      layer % activation_type = 1
      layer % activation_str = 'linear'
    else if (trim(activation) == 'gaussian') then
      layer % activation_type = 2
      layer % activation_str = 'gaussian'
    else if (trim(activation) == 'relu') then
      layer % activation_type = 3
      layer % activation_str = 'relu'
    else if (trim(activation) == 'sigmoid') then
      layer % activation_type = 4
      layer % activation_str = 'sigmoid'
    else if (trim(activation) == 'step') then
      layer % activation_type = 5
      layer % activation_str = 'step'
    else if (trim(activation) == 'tanh') then
      layer % activation_type = 6
      layer % activation_str = 'tanh'
    else
      layer % activation_type = 4
      layer % activation_str = 'sigmoid'
    end if
  end subroutine layer_set_activation

  pure function layer_activation(layer, x) result(res)
    ! Applies the activation function based on the stored type.
    type(layer_type), intent(in) :: layer
    real, intent(in) :: x(:)
    real :: res(size(x))
    if (layer % activation_type == 1) then
      res = linear(x)
    else if (layer % activation_type == 2) then
      res = gaussian(x)
    else if (layer % activation_type == 3) then
      res = relu(x)
    else if (layer % activation_type == 4) then
      res = sigmoid(x)
    else if (layer % activation_type == 5) then
      res = step(x)
    else if (layer % activation_type == 6) then
      res = tanhf(x)
    else
      res = sigmoid(x)
    end if
  end function layer_activation

  pure function layer_activation_prime(layer, x) result(res)
    ! Applies the activation function derivative based on the stored type.
    type(layer_type), intent(in) :: layer
    real, intent(in) :: x(:)
    real :: res(size(x))
    if (layer % activation_type == 1) then
      res = linear_prime(x)
    else if (layer % activation_type == 2) then
      res = gaussian_prime(x)
    else if (layer % activation_type == 3) then
      res = relu_prime(x)
    else if (layer % activation_type == 4) then
      res = sigmoid_prime(x)
    else if (layer % activation_type == 5) then
      res = step_prime(x)
    else if (layer % activation_type == 6) then
      res = tanh_prime(x)
    else
      res = sigmoid_prime(x)
    end if
  end function layer_activation_prime

end module mod_layer