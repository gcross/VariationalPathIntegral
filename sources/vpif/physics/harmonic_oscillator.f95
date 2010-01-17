!@+leo-ver=4-thin
!@+node:gcross.20091227115154.1339:@thin harmonic_oscillator.f95
!@@language fortran90

module vpif__physics__harmonic_oscillator
  implicit none

contains

!@+others
!@+node:gcross.20091212120817.1281:compute_potential
pure subroutine compute_potential( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    potential_coefficients, &
    particle_positions, &
    potential &
)
    integer, intent(in) :: number_of_slices, number_of_particles, number_of_dimensions
    double precision, intent(in) :: &
        potential_coefficients(number_of_dimensions), &
        particle_positions(number_of_dimensions, number_of_particles, number_of_slices)
    double precision, intent(out) :: &
        potential(number_of_slices)

    integer :: particle_number, slice_number
    double precision :: temp(number_of_dimensions)

    do slice_number = 1, number_of_slices
        temp = 0
        do particle_number = 1, number_of_particles
            temp = temp + particle_positions(:,particle_number,slice_number)**2
        end do
        potential(slice_number) = dot_product(temp,potential_coefficients)/2d0
    end do

end subroutine
!@-node:gcross.20091212120817.1281:compute_potential
!@+node:gcross.20091212120817.1279:compute_trial_weight
pure function compute_trial_weight( &
    number_of_particles, number_of_dimensions, &
    trial_coefficients, &
    particle_positions &
) result (weight)
    integer, intent(in) :: number_of_particles, number_of_dimensions
    double precision, intent(in) :: &
        particle_positions(number_of_dimensions, number_of_particles), &
        trial_coefficients(number_of_dimensions)
    double precision :: weight

    double precision :: temp(number_of_dimensions)
    integer :: i

    temp = 0
    do i = 1, number_of_particles
        temp = temp + particle_positions(:,i)**2
    end do

    weight = -dot_product(temp,sqrt(trial_coefficients))/2d0

end function
!@-node:gcross.20091212120817.1279:compute_trial_weight
!@+node:gcross.20100106124611.2033:compute_trial_weight_
pure subroutine compute_trial_weight_( &
    number_of_particles, number_of_dimensions, &
    trial_coefficients, &
    particle_positions, &
    weight &
)
    integer, intent(in) :: number_of_particles, number_of_dimensions
    double precision, intent(in) :: &
        particle_positions(number_of_dimensions, number_of_particles), &
        trial_coefficients(number_of_dimensions)
    double precision, intent(out) :: weight

    weight = &
        compute_trial_weight( &
            number_of_particles, number_of_dimensions, &
            trial_coefficients, &
            particle_positions &
        )
end subroutine
!@-node:gcross.20100106124611.2033:compute_trial_weight_
!@+node:gcross.20091212120817.1280:compute_trial_derivatives
pure subroutine compute_trial_derivatives( &
    number_of_particles, number_of_dimensions, &
    trial_coefficients, &
    particle_positions, &
    gradient_of_log_trial_fn, laplacian_of_log_trial_fn &
)
    integer, intent(in) :: number_of_particles, number_of_dimensions
    double precision, intent(in) :: &
        particle_positions(number_of_dimensions, number_of_particles), &
        trial_coefficients(number_of_dimensions)
    double precision, intent(out) :: &
        gradient_of_log_trial_fn(number_of_dimensions, number_of_particles), &
        laplacian_of_log_trial_fn

    integer :: i
    double precision :: sqrt_trial_coefficients(number_of_dimensions)

    sqrt_trial_coefficients = sqrt(trial_coefficients)

    forall (i = 1:number_of_particles) &
        gradient_of_log_trial_fn(:,i) = -sqrt_trial_coefficients(:)*particle_positions(:,i)

    laplacian_of_log_trial_fn = -sum(sqrt_trial_coefficients)*number_of_particles

end subroutine
!@-node:gcross.20091212120817.1280:compute_trial_derivatives
!@-others

end module
!@-node:gcross.20091227115154.1339:@thin harmonic_oscillator.f95
!@-leo
