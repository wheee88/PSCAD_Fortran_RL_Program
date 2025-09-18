module mod_activation

  ! A collection of activation functions and their derivatives.

  implicit none

  private

  public :: activation_function
  public :: linear, linear_prime
  public :: gaussian, gaussian_prime
  public :: relu, relu_prime
  public :: sigmoid, sigmoid_prime
  public :: step, step_prime
  public :: tanhf, tanh_prime

  interface
    pure function activation_function(x)
      real, intent(in) :: x(:)
      real :: activation_function(size(x))
    end function activation_function
  end interface

contains

    pure function linear(x) result(res)
    ! Linear activation function(means no activation function).
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = x
  end function linear

  pure function linear_prime(x) result(res)
    ! First derivative of the Gaussian activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = 1.0
  end function linear_prime
  
  pure function gaussian(x) result(res)
    ! Gaussian activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = exp(-x**2)
  end function gaussian

  pure function gaussian_prime(x) result(res)
    ! First derivative of the Gaussian activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = -2.0 * x * gaussian(x)
  end function gaussian_prime

  pure function relu(x) result(res)
    !! REctified Linear Unit (RELU) activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = max(0.0, x)
  end function relu

  pure function relu_prime(x) result(res)
    ! First derivative of the REctified Linear Unit (RELU) activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    where (x > 0)
      res = 1.0
    elsewhere
      res = 0.0
    end where
  end function relu_prime

  pure function sigmoid(x) result(res)
    ! Sigmoid activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = 1.0 / (1.0 + exp(-x))
  end function sigmoid

  pure function sigmoid_prime(x) result(res)
    ! First derivative of the sigmoid activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = sigmoid(x) * (1.0 - sigmoid(x))
  end function sigmoid_prime

  pure function step(x) result(res)
    ! Step activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    where (x > 0)
      res = 1.0
    elsewhere
      res = 0.0
    end where
  end function step

  pure function step_prime(x) result(res)
    ! First derivative of the step activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = 0.0
  end function step_prime

  pure function tanhf(x) result(res)
    ! Tangent hyperbolic activation function. 
    ! Same as the intrinsic tanh, but must be 
    ! defined here so that we can use procedure
    ! pointer with it.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = tanh(x)
  end function tanhf

  pure function tanh_prime(x) result(res)
    ! First derivative of the tanh activation function.
    real, intent(in) :: x(:)
    real :: res(size(x))
    res = 1.0 - tanh(x)**2
  end function tanh_prime

end module mod_activation
