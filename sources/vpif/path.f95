!@+leo-ver=4-thin
!@+node:gcross.20091216150502.1725:@thin path.f95
!@@language fortran90

module vpif__path
  implicit none

contains

!@+others
!@+node:gcross.20091216150502.1730:compute_separations
subroutine compute_separations( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    path_positions, &
    path_separations &
)
    integer, intent(in) :: number_of_dimensions, number_of_particles, number_of_slices
    double precision, intent(in) :: &
        path_positions(number_of_dimensions, number_of_particles, number_of_slices)

    double precision, intent(out) :: &
        path_separations(number_of_particles, number_of_particles, number_of_slices)

    integer :: i, j1, j2
    double precision :: d

    do i = 1, number_of_slices
        do j1 = 1, number_of_particles
            path_separations(j1,j1,i) = 0
            do j2 = j1+1, number_of_particles
                d = sqrt(sum((path_positions(:,j1,i)-path_positions(:,j2,i))**2))
                path_separations(j2,j1,i) = d
                path_separations(j1,j2,i) = d
            end do
        end do
    end do

end subroutine
!@-node:gcross.20091216150502.1730:compute_separations
!@+node:gcross.20091216150502.1727:create_initial_path
subroutine create_initial_path( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    lower_bounds, upper_bounds, &
    path_positions, path_separations &
)
    integer, intent(in) :: number_of_dimensions, number_of_particles, number_of_slices
    double precision, intent(in) :: lower_bounds(number_of_dimensions), upper_bounds(number_of_dimensions)

    double precision, intent(out) :: &
        path_positions(number_of_dimensions, number_of_particles, number_of_slices), &
        path_separations(number_of_particles, number_of_particles, number_of_slices)

    double precision :: scale_factor(number_of_dimensions)
    integer :: i, j, k

    scale_factor = upper_bounds-lower_bounds

    do i = 1, number_of_slices
    do j = 1, number_of_particles
    do k = 1, number_of_dimensions
        path_positions(k,j,i) = rand() * scale_factor(k) + lower_bounds(k)
    end do
    end do
    end do

    call compute_separations( &
        number_of_slices, number_of_particles, number_of_dimensions, &
        path_positions, &
        path_separations &
    )

end subroutine
!@-node:gcross.20091216150502.1727:create_initial_path
!@-others

end module
!@-node:gcross.20091216150502.1725:@thin path.f95
!@-leo
