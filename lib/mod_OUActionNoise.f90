module mod_OUActionNoise
    use mod_random, only: randn
    implicit none
    
    private
    public :: noise_type, noise_constructor, noise_init, noise_call
    
    type :: noise_type
        real :: mean, std_dev, theta, dt, x_prev
    end type noise_type

contains   
    function noise_constructor(mean, std_deviation) result(ou_noise)
        implicit none
        real, intent(in) :: mean, std_deviation
        type(noise_type) :: ou_noise
        real :: theta, dt, x_prev
        theta = 0.15
        dt = 0.01
        x_prev = 0.0
        call noise_init(ou_noise, mean, std_deviation, theta, dt, x_prev)
    end function noise_constructor
    
    subroutine noise_init(ou_noise, mean, std_deviation, theta, dt, x_prev)
        implicit none
        type(noise_type), intent(in out) :: ou_noise
        real, intent(in) :: mean, std_deviation, theta, dt, x_prev
        ou_noise%theta = theta
        ou_noise%mean = mean
        ou_noise%std_dev = std_deviation
        ou_noise%dt = dt
        ou_noise%x_prev = x_prev
    end subroutine noise_init
    
    real function noise_call(ou_noise) result(x)
        implicit none
        type(noise_type), intent(in out) :: ou_noise
        real :: rand(1)
        rand = randn(1)
        x = ou_noise%x_prev + ou_noise%theta * (ou_noise%mean - ou_noise%x_prev) * ou_noise%dt + &
            ou_noise%std_dev * sqrt(ou_noise%dt) * rand(1)
        ! Store x into x_prev
        ! Makes next noise dependent on current one
        ou_noise%x_prev = x
    end function noise_call
end module mod_OUActionNoise