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
//@+node:gcross.20100111122429.1490:update_path
void vpic__path__update_path(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    int update_start_slice, int update_end_slice,
    double old_particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double old_particle_separations[number_of_slices][number_of_particles][number_of_particles],
    double updated_particle_positions[][number_of_particles][number_of_dimensions],
    double updated_particle_separations[][number_of_particles][number_of_dimensions],
    double new_particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double new_particle_separations[number_of_slices][number_of_particles][number_of_particles]
) {__vpif__path_MOD_update_path(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    &update_start_slice, &update_end_slice,
    old_particle_positions,
    old_particle_separations,
    updated_particle_positions,
    updated_particle_separations,
    new_particle_positions,
    new_particle_separations
);}
//@-node:gcross.20100111122429.1490:update_path
//@-others
//@-node:gcross.20091216150502.1728:@thin path.c
//@-leo
