//@+leo-ver=4-thin
//@+node:gcross.20091227115154.1319:@thin moves.c
//@@language C

//@+others
//@+node:gcross.20091227115154.1320:rigid
void vpi__path__moves__rigid(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    int particle_number_to_shift,
    double maximum_shift,
    double old_particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double new_particle_positions[number_of_slices][number_of_particles][number_of_dimensions]
) {
__vpi__path__moves_MOD_rigid(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    &particle_number_to_shift,
    &maximum_shift,
    old_particle_positions,
    new_particle_positions
);
}
//@-node:gcross.20091227115154.1320:rigid
//@-others
//@-node:gcross.20091227115154.1319:@thin moves.c
//@-leo
