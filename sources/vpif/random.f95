!@+leo-ver=4-thin
!@+node:gcross.20091217090302.1298:@thin random.f95
!@@language fortran90

module vpi__random
  implicit none

contains

!@+others
!@+node:gcross.20091217090302.1300:sample_unit_normal_distribution
subroutine sample_unit_normal_distribution(size,sample)
    integer, intent(in) :: size
    double precision, intent(out) :: sample
    double precision :: fac, v(2)

    integer:: i

    do i = 1, (size-1), 2
        call compute_fac()
        sample(i) = v(1) * fac
        sample(i+1) = v(2) * fac
    end do

    if (mod(size,2) == 1) then
        call compute_fac()
        sample(size) = v(1) * fac
    end if

  contains

    subroutine compute_fac()
        double precision :: rsq

        rsq = 0.0
        do while ( (rsq .ge. 1) .or. (rsq .eq. 0.0) )
          call random_number( v )
          v(:) = 2.0*v(:) - 1.0
          rsq = dot_product(v,v)
        end do
        fac = sqrt(-2.0 * log(rsq) / rsq)
    end function

end subroutine ru_gasdev
!@-node:gcross.20091217090302.1300:sample_unit_normal_distribution
!@-others

end module
!@-node:gcross.20091217090302.1298:@thin random.f95
!@-leo
