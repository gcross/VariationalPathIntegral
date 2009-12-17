//@+leo-ver=4-thin
//@+node:gcross.20091216150502.1728:@thin path.c
//@@language C

//@+others
//@+node:gcross.20091216150502.1729:create_initial_path
void vpi__path__create_initial_path(
    int number_of_dimensions, int number_of_particles, int number_of_slices,
    double lower_bounds[], double upper_bounds[],
    double path_positions[], double path_separations[]
) {
__vpi__path_MOD_create_initial_path(
    &number_of_dimensions, &number_of_particles, &number_of_slices,
    lower_bounds, upper_bounds,
    path_positions, path_separations
);
}
//@-node:gcross.20091216150502.1729:create_initial_path
//@-others
//@-node:gcross.20091216150502.1728:@thin path.c
//@-leo
