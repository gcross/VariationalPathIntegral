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
  double precision, dimension( number_of_dimensions ), intent(in) :: potential_coefficients
  double precision, dimension( number_of_dimensions, number_of_particles, number_of_slices ), intent(in) :: particle_positions
  double precision, dimension( number_of_particles, number_of_slices ), intent(out)  :: potential

  integer :: i,j

  do i = 1, number_of_slices
  do j = 1, number_of_particles
    potential(j,i) = dot_product(potential_coefficients,particle_positions(:,j,i)**2)/2d0
  end do
  end do

end subroutine
!@-node:gcross.20091212120817.1281:compute_potential
!@+node:gcross.20091212120817.1279:compute_trial_weight
pure function compute_trial_weight( &
    number_of_particles, number_of_dimensions, &
    trial_coefficients, &
    particle_positions &
  ) result ( weight )
  integer, intent(in) :: number_of_particles, number_of_dimensions
  double precision, dimension( number_of_dimensions, number_of_particles ), intent(in) :: particle_positions
  double precision, dimension( number_of_dimensions ), intent(in) :: trial_coefficients
  double precision  :: weight

  double precision, dimension( number_of_dimensions ) :: temp
  integer :: i

  temp = 0
  do i = 1, number_of_particles
    temp = temp + particle_positions(:,i)**2
  end do

  weight = -dot_product(temp,trial_coefficients)/2d0

end function
!@-node:gcross.20091212120817.1279:compute_trial_weight
!@+node:gcross.20091212120817.1280:compute_trial_derivatives
pure subroutine compute_trial_derivatives( &
    number_of_particles, number_of_dimensions, &
    trial_coefficients, &
    particle_positions, &
    gradient_of_log_trial_fn, laplacian_of_log_trial_fn &
  )
  integer, intent(in) :: number_of_particles, number_of_dimensions
  double precision, dimension( number_of_dimensions, number_of_particles ), intent(in) :: particle_positions
  double precision, dimension( number_of_dimensions ), intent(in) :: trial_coefficients
  double precision, dimension( number_of_dimensions, number_of_particles ), intent(out) :: gradient_of_log_trial_fn
  double precision, intent(out) :: laplacian_of_log_trial_fn

  integer :: i

  forall (i = 1:number_of_particles) &
    gradient_of_log_trial_fn(:,i) = -trial_coefficients(:)*particle_positions(:,i)

  laplacian_of_log_trial_fn = -sum(trial_coefficients)*number_of_particles
end subroutine
!@-node:gcross.20091212120817.1280:compute_trial_derivatives
!@-others

end module
!@-node:gcross.20091227115154.1339:@thin harmonic_oscillator.f95
!@-leo
