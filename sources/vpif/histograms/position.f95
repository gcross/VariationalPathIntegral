!@+leo-ver=4-thin
!@+node:gcross.20100109140101.1540:@thin position.f95
!@@language fortran90

module vpif__histograms__position
  implicit none

contains

!@+others
!@+node:gcross.20100109140101.1541:bin_all_1d_integrated_slices
subroutine bin_all_1d_integrated_slices(&
    number_of_particles, number_of_dimensions, &
    particle_positions, &
    number_of_bins, &
    lower_bounds, upper_bounds, &
    histogram &
)
    integer, intent(in) :: number_of_particles, number_of_dimensions, number_of_bins
    double precision, intent(in) :: &
        particle_positions(number_of_dimensions,number_of_particles), &
        lower_bounds(number_of_dimensions), &
        upper_bounds(number_of_dimensions)
    integer, intent(inout) :: &
        histogram(number_of_bins,number_of_dimensions)

    double precision :: bin_factors(number_of_dimensions)
    integer :: particle_number, d, bin

    bin_factors = number_of_bins / (upper_bounds - lower_bounds)

    do particle_number = 1, number_of_particles
        do d = 1, number_of_dimensions
            bin = floor((particle_positions(d,particle_number) - lower_bounds(d)) * bin_factors(d)) + 1
            histogram(bin,d) = histogram(bin,d) + 1
        end do
    end do
end subroutine
!@-node:gcross.20100109140101.1541:bin_all_1d_integrated_slices
!@-others

end module
!@-node:gcross.20100109140101.1540:@thin position.f95
!@-leo
