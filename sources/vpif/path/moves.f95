!@+leo-ver=4-thin
!@+node:gcross.20091217090302.1302:@thin moves.f95
!@@language fortran90

module vpif__path__moves
  use vpif__random
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
!@+node:gcross.20100103202029.1826:brownian_bridge_implementation
subroutine brownian_bridge_implementation( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    particle_number_to_move, &
    move_leftmost_slice, move_rightmost_slice, &
    ignore_leftmost_slice, &
    hbar_over_2m, time_interval, &
    old_particle_positions, &
    new_particle_positions &
)
    integer, intent(in) :: &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_move
    logical, intent(in) :: &
        move_leftmost_slice, move_rightmost_slice, &
        ignore_leftmost_slice
    double precision, intent(in) :: &
        hbar_over_2m, time_interval, &
old_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)
    double precision, intent(out) :: &
new_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)

    integer :: &
        current_random_number, &
        slice_number, dimension_
    double precision :: &
        dt, &
gaussian_random_numbers(number_of_slices*number_of_dimensions), &
        t1, t2, &
        a, b

    dt = hbar_over_2m*time_interval*2.0d0

    new_particle_positions(:,:particle_number_to_move-1,:) = &
    old_particle_positions(:,:particle_number_to_move-1,:)

    new_particle_positions(:,particle_number_to_move+1:,:) = &
    old_particle_positions(:,particle_number_to_move+1:,:)

    call sample_unit_normal_distribution(size(gaussian_random_numbers),gaussian_random_numbers)
    current_random_number = 1

    if (move_leftmost_slice) then
        call apply_endpoint_move(1,sqrt(dt*(number_of_slices-1)))
    else if (.not. ignore_leftmost_slice) then
        new_particle_positions(:,particle_number_to_move,1) =&
        old_particle_positions(:,particle_number_to_move,1)
    end if
    if (move_rightmost_slice) then
        call apply_endpoint_move(number_of_slices,sqrt(dt*(number_of_slices-1)))
    else
        new_particle_positions(:,particle_number_to_move,number_of_slices) =&
        old_particle_positions(:,particle_number_to_move,number_of_slices)
    end if

    do slice_number = 2, number_of_slices-1
        t1 = dt
        t2 = dt*(number_of_slices-slice_number)
        do dimension_ = 1, number_of_dimensions
            a = new_particle_positions(dimension_,particle_number_to_move,slice_number-1)
            b = new_particle_positions(dimension_,particle_number_to_move,number_of_slices)
            call apply_move( &
                slice_number, &
                dimension_, &
                (t2*a  + t1*b)/(t1 + t2), &
                sqrt(1./( 1./t1 + 1./t2)) &
            )
        end do
    end do

contains

    subroutine apply_endpoint_move(slice_number, sigma)
        integer, intent(in) :: slice_number
        double precision, intent(in) :: sigma

        integer :: dimension_

        do dimension_ = 1, number_of_dimensions
            call apply_move( &
                slice_number, &
                dimension_, &
old_particle_positions(dimension_,particle_number_to_move,slice_number), &
                sigma &
            )
        end do
    end subroutine

    subroutine apply_move(slice_number, dimension_, mean, sigma)
        integer, intent(in) :: slice_number, dimension_
        double precision, intent(in) :: mean, sigma

new_particle_positions(dimension_,particle_number_to_move,slice_number) = &
            gaussian_random_numbers(current_random_number)*sigma + mean
        current_random_number = current_random_number + 1
    end subroutine

end subroutine

!@-node:gcross.20100103202029.1826:brownian_bridge_implementation
!@+node:gcross.20100116114537.2108:brownian_bridge
subroutine brownian_bridge( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    particle_number_to_move, &
    move_leftmost_slice, move_rightmost_slice, &
    hbar_over_2m, time_interval, &
    old_particle_positions, &
    new_particle_positions &
)
    integer, intent(in) :: &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_move
    logical, intent(in) :: &
        move_leftmost_slice, move_rightmost_slice
    double precision, intent(in) :: &
        hbar_over_2m, time_interval, &
old_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)
    double precision, intent(out) :: &
new_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)

    call brownian_bridge_implementation( &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_move, &
        move_leftmost_slice, move_rightmost_slice, &
        .false., &
        hbar_over_2m, time_interval, &
        old_particle_positions, &
        new_particle_positions &
    )

end subroutine

!@-node:gcross.20100116114537.2108:brownian_bridge
!@+node:gcross.20100116114537.2096:linked_brownian_bridge
subroutine linked_brownian_bridge( &
    number_of_slices, number_of_particles, number_of_dimensions, &
    particle_number_to_move, &
    move_leftmost_slice, move_rightmost_slice, &
    link_slice_number, &
    duplicate_link_slice, &
    hbar_over_2m, time_interval, &
    old_particle_positions, &
    new_particle_positions &
)
    integer, intent(in) :: &
        number_of_slices, number_of_particles, number_of_dimensions, &
        particle_number_to_move, &
        link_slice_number
    logical, intent(in) :: &
        move_leftmost_slice, move_rightmost_slice, &
        duplicate_link_slice
    double precision, intent(in) :: &
        hbar_over_2m, time_interval, &
old_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)
    double precision, intent(out) :: &
new_particle_positions(number_of_dimensions,number_of_particles,number_of_slices)

    integer :: left_bridge_length, right_bridge_length
    logical :: move_other_side_of_link_slice

    left_bridge_length = link_slice_number
    call brownian_bridge_implementation( &
        left_bridge_length, number_of_particles, number_of_dimensions, &
        particle_number_to_move, &
        move_leftmost_slice, .true., &
        .false., &
        hbar_over_2m, time_interval, &
        old_particle_positions(:,:,:left_bridge_length), &
        new_particle_positions(:,:,:left_bridge_length) &
    )

    right_bridge_length = number_of_slices - left_bridge_length
    if (duplicate_link_slice) then
        move_other_side_of_link_slice = .false.
        new_particle_positions(:,:,link_slice_number+1) = new_particle_positions(:,:,link_slice_number)
    else
        move_other_side_of_link_slice = .true.
    end if    
    call brownian_bridge_implementation( &
        right_bridge_length, number_of_particles, number_of_dimensions, &
        particle_number_to_move, &
        move_other_side_of_link_slice, move_rightmost_slice, &
        .not. move_other_side_of_link_slice, &
        hbar_over_2m, time_interval, &
        old_particle_positions(:,:,link_slice_number+1:), &
        new_particle_positions(:,:,link_slice_number+1:) &
    )

end subroutine

!@-node:gcross.20100116114537.2096:linked_brownian_bridge
!@-others

end module
!@-node:gcross.20091217090302.1302:@thin moves.f95
!@-leo
