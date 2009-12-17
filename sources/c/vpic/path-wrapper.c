#@+leo-ver=4-thin
#@+node:gcross.20091216150502.1728:@thin path-wrapper.c
#@+others
#@+node:gcross.20091216150502.1729:create_initial_path
extern void __vpi__path__create_initial_path_(
    int* number_of_dimensions, int* number_of_particles, int* number_of_slices,
    double lower_bounds[], double upper_bounds[],
    double path[]
)

void vpi__path__create_initial_path(
    int number_of_dimensions, int number_of_particles, int number_of_slices,
    double lower_bounds[], double upper_bounds[],
    double path[]
) {
__vpi__path__create_initial_path_(
    &number_of_dimensions, &number_of_particles, &number_of_slices,
    double lower_bounds, double upper_bounds,
    double path
)
}
#@-node:gcross.20091216150502.1729:create_initial_path
#@-others
#@-node:gcross.20091216150502.1728:@thin path-wrapper.c
#@-leo
