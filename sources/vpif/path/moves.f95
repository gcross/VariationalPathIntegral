!@+leo-ver=4-thin
!@+node:gcross.20091217090302.1302:@thin moves.f95
!@@language fortran90

module vpi__path__moves
  implicit none

contains

!@+others
!@+node:gcross.20091217090302.1303:rigid
subroutine rigid( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    particle_number_to_shift, &
    maximum_shift, &
    old_particle_positions, &
    new_particle_positions &
)
    integer, intent(in) :: &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_shift
    double precision, intent(in) :: &
        maximum_shift, &
        old_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)
    double precision, intent(out) :: &
        new_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)

    double precision :: shifts(number_of_dimensions)
    integer :: i

    call random_number(shifts)
    shifts = (shifts - 0.5) * 2 * maximum_shift

    new_particle_positions = old_particle_positions
    forall (i = 1:number_of_slices) &
        new_particle_positions(:,particle_number_to_shift,i) = &
        new_particle_positions(:,particle_number_to_shift,i) + shifts

end subroutine
!@-node:gcross.20091217090302.1303:rigid
!@-others

end module
!@-node:gcross.20091217090302.1302:@thin moves.f95
!@-leo
