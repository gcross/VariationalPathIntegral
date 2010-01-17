//@+leo-ver=4-thin
//@+node:gcross.20091227115154.1319:@thin moves.c
//@@language C

//@+others
//@+node:gcross.20091227115154.1320:rigid
void vpic__path__moves__rigid(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    int particle_number_to_shift,
    double maximum_shift,
    double old_particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double new_particle_positions[number_of_slices][number_of_particles][number_of_dimensions]
) {
__vpif__path__moves_MOD_rigid(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    &particle_number_to_shift,
    &maximum_shift,
    old_particle_positions,
    new_particle_positions
);
}
//@-node:gcross.20091227115154.1320:rigid
//@+node:gcross.20100116114537.1616:brownian_bridge
void vpic__path__moves__brownian_bridge(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    int particle_number_to_move,
    int endpoint_move_mode,
    int center_slice_mode,
    int center_slice_number,
    double hbar_over_2m, double time_interval,
    double old_particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double new_particle_positions[number_of_slices][number_of_particles][number_of_dimensions]
) {__vpif__path__moves_MOD_brownian_bridge(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    &particle_number_to_move,
    &endpoint_move_mode,
    &center_slice_mode,
    &center_slice_number,
    &hbar_over_2m, &time_interval,
    old_particle_positions,
    new_particle_positions
);}
//@-node:gcross.20100116114537.1616:brownian_bridge
//@-others
//@-node:gcross.20091227115154.1319:@thin moves.c
//@-leo
