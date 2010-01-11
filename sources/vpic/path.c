//@+leo-ver=4-thin
//@+node:gcross.20091216150502.1728:@thin path.c
//@@language C

//@+others
//@+node:gcross.20091226065853.1632:compute_separations
void vpic__path__compute_separations(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    double path_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double path_separations[number_of_slices][number_of_particles][number_of_particles]
) {
__vpif__path_MOD_compute_separations(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    path_positions, path_separations
);
}
//@nonl
//@-node:gcross.20091226065853.1632:compute_separations
//@+node:gcross.20091216150502.1729:create_initial_path
void vpic__path__create_initial_path(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    double lower_bounds[number_of_dimensions], double upper_bounds[number_of_dimensions],
    double path_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double path_separations[number_of_slices][number_of_particles][number_of_particles]
) {
__vpif__path_MOD_create_initial_path(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    lower_bounds, upper_bounds,
    path_positions,
    path_separations
);
}
//@-node:gcross.20091216150502.1729:create_initial_path
//@-others
//@-node:gcross.20091216150502.1728:@thin path.c
//@-leo
