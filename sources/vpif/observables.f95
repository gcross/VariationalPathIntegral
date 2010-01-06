!@+leo-ver=4-thin
!@+node:gcross.20100105133218.1370:@thin observables.f95
!@@language fortran90

module vpif__observables
  implicit none

contains

!@+others
!@+node:gcross.20100105133218.1372:compute_energy
function compute_energy( &
    number_of_particles, number_of_dimensions, &
    hbar_over_2m, &
    potential, &
    gradient_of_log_trial_fn, &
    laplacian_of_log_trial_fn &
) result (energy)
    integer, intent(in) :: number_of_dimensions, number_of_particles
    double precision, intent(in) :: &
        hbar_over_2m, &
        potential, &
        gradient_of_log_trial_fn(number_of_dimensions, number_of_particles), &
        laplacian_of_log_trial_fn

    double precision :: energy

    energy = potential - hbar_over_2m * ( sum(gradient_of_log_trial_fn(:,:)**2) + laplacian_of_log_trial_fn )

end function
!@-node:gcross.20100105133218.1372:compute_energy
!@+node:gcross.20100105133218.1568:compute_energy_subroutine
subroutine compute_energy_subroutine( &
    number_of_particles, number_of_dimensions, &
    hbar_over_2m, &
    potential, &
    gradient_of_log_trial_fn, &
    laplacian_of_log_trial_fn, &
    energy &
)
    integer, intent(in) :: number_of_dimensions, number_of_particles
    double precision, intent(in) :: &
        hbar_over_2m, &
        potential, &
        gradient_of_log_trial_fn(number_of_dimensions, number_of_particles), &
        laplacian_of_log_trial_fn
    double precision, intent(out) :: energy

    energy = compute_energy( &
        number_of_particles, number_of_dimensions, &
        hbar_over_2m, &
        potential, &
        gradient_of_log_trial_fn, &
        laplacian_of_log_trial_fn &
    )

end subroutine
!@-node:gcross.20100105133218.1568:compute_energy_subroutine
!@-others

end module
!@-node:gcross.20100105133218.1370:@thin observables.f95
!@-leo
