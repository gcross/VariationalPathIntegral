!@+leo-ver=4-thin
!@+node:gcross.20091217090302.1302:@thin moves.f95
!@@language fortran90

module vpif__path__moves
  use vpif__random
  implicit none

!@<< Constants >>
!@+node:gcross.20100117030121.2096:<< Constants >>
integer, parameter :: &
    move_neither_endpoint = 0, &
    move_left_endpoint = 1, &
    move_right_endpoint = 2

integer, parameter :: &
    no_center_slice = 0, &
    duplicated_center_slice = 1, &
    disconnected_center_slice = 2
!@nonl
!@-node:gcross.20100117030121.2096:<< Constants >>
!@nl

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
!@+node:gcross.20100103202029.1826:brownian_bridge
subroutine brownian_bridge( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    particle_number_to_move, &
    endpoint_move_mode, &
    center_slice_mode, center_slice_number, &
    hbar_over_2m, time_interval, &
    old_particle_positions, &
    new_particle_positions &
)
    integer, intent(in) :: &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_move, &
        endpoint_move_mode, &
        center_slice_mode, center_slice_number
    double precision, intent(in) :: &
        hbar_over_2m, time_interval, &
old_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)
    double precision, intent(out) :: &
new_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)

    integer :: current_random_number
    double precision :: &
        dt, &
gaussian_random_numbers(number_of_slices*number_of_dimensions)

    dt = hbar_over_2m*time_interval*2.0d0

    new_particle_positions(:,:particle_number_to_move-1,:) = &
    old_particle_positions(:,:particle_number_to_move-1,:)

    new_particle_positions(:,particle_number_to_move+1:,:) = &
    old_particle_positions(:,particle_number_to_move+1:,:)

    call sample_unit_normal_distribution(size(gaussian_random_numbers),gaussian_random_numbers)
    current_random_number = 1

    select case (endpoint_move_mode)
        case (move_neither_endpoint)
            new_particle_positions(:,particle_number_to_move,1) =&
            old_particle_positions(:,particle_number_to_move,1)
            new_particle_positions(:,particle_number_to_move,number_of_slices) =&
            old_particle_positions(:,particle_number_to_move,number_of_slices)
        case (move_left_endpoint)
            new_particle_positions(:,particle_number_to_move,number_of_slices) =&
            old_particle_positions(:,particle_number_to_move,number_of_slices)
            call move(1,number_of_slices,sqrt(dt*(number_of_slices-1)))
        case (move_right_endpoint)
            new_particle_positions(:,particle_number_to_move,1) =&
            old_particle_positions(:,particle_number_to_move,1)
            call move(number_of_slices,1,sqrt(dt*(number_of_slices-1)))
    end select

    select case (center_slice_mode)
        case (no_center_slice)
            call bridge(1,number_of_slices)
        case (duplicated_center_slice)
            call interpolate(center_slice_number, &
                             center_slice_number-1, 1, &
                             number_of_slices - (center_slice_number+1), number_of_slices &
                            )
            new_particle_positions(:,particle_number_to_move,center_slice_number+1) = &
            new_particle_positions(:,particle_number_to_move,center_slice_number)
            call bridge(1,center_slice_number)
            call bridge(center_slice_number+1,number_of_slices)
        case (disconnected_center_slice)
            call move(center_slice_number,1,sqrt(dt*(center_slice_number-1)))
            call move(center_slice_number+1,number_of_slices,sqrt(dt*(number_of_slices-(center_slice_number+1))))
            call bridge(1,center_slice_number)
            call bridge(center_slice_number+1,number_of_slices)
    end select

contains

    subroutine move(slice_number_to_modify, slice_number_to_lookup, sigma)
        integer, intent(in) :: slice_number_to_modify, slice_number_to_lookup
        double precision, intent(in) :: sigma

        integer :: dimension_
        double precision :: mean

        do dimension_ = 1, number_of_dimensions
            mean = new_particle_positions(dimension_,particle_number_to_move,slice_number_to_lookup)
            new_particle_positions(dimension_,particle_number_to_move,slice_number_to_modify) =  &
                gaussian_random_numbers(current_random_number)*sigma + mean
            current_random_number = current_random_number + 1
        end do
    end subroutine

    subroutine bridge(bridge_start_slice,bridge_end_slice)
        integer, intent(in) :: bridge_start_slice, bridge_end_slice
        integer :: slice_number
        do slice_number = bridge_start_slice+1, bridge_end_slice-1
            call interpolate(slice_number, &
                             1,slice_number-1, &
                             (bridge_end_slice-slice_number),bridge_end_slice &
                            )
        end do
    end subroutine

    subroutine interpolate(slice_number,left_time_interval,left_slice_number,right_time_interval,right_slice_number)
        integer, intent(in) :: &
            slice_number, &
            left_time_interval, left_slice_number, &
            right_time_interval, right_slice_number
        double precision :: t1, t2, a, b, mean, sigma
        integer :: dimension_
        t1 = dt*left_time_interval
        t2 = dt*right_time_interval
        sigma = sqrt(1./( 1./t1 + 1./t2))
        do dimension_ = 1, number_of_dimensions
            a = new_particle_positions(dimension_,particle_number_to_move,left_slice_number)
            b = new_particle_positions(dimension_,particle_number_to_move,right_slice_number)
            mean = (t2*a + t1*b)/(t1 + t2)
            new_particle_positions(dimension_,particle_number_to_move,slice_number) = &
                gaussian_random_numbers(current_random_number)*sigma + mean
            current_random_number = current_random_number + 1
        end do
    end subroutine

end subroutine

!@-node:gcross.20100103202029.1826:brownian_bridge
!@-others

end module
!@-node:gcross.20091217090302.1302:@thin moves.f95
!@-leo
