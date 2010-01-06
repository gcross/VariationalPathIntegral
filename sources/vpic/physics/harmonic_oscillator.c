//@+leo-ver=4-thin
//@+node:gcross.20091227115154.1344:@thin harmonic_oscillator.c
//@@language C

//@+others
//@+node:gcross.20091212120817.1288:compute_potential
void vpic__physics__harmonic_oscillator__compute_potential(
    int number_of_slices, int number_of_particles, int number_of_dimensions,
    double potential_coefficients[number_of_dimensions],
    double particle_positions[number_of_slices][number_of_particles][number_of_dimensions],
    double potential[number_of_slices]
){__vpif__physics__harmonic_oscillator_MOD_compute_potential(
    &number_of_slices, &number_of_particles, &number_of_dimensions,
    potential_coefficients,
    particle_positions,
    potential
);}
//@-node:gcross.20091212120817.1288:compute_potential
//@+node:gcross.20091227115154.1346:compute_trial_weight
double vpic__physics__harmonic_oscillator__compute_trial_weight(
    int number_of_particles, int number_of_dimensions,
    double trial_coefficients[number_of_dimensions],
    double particle_positions[number_of_particles][number_of_dimensions]
){
    double weight;
    __vpif__physics__harmonic_oscillator_MOD_compute_trial_weight(
        &number_of_particles, &number_of_dimensions,
        trial_coefficients,
        particle_positions,
        &weight
    );
    return weight;
}
//@-node:gcross.20091227115154.1346:compute_trial_weight
//@+node:gcross.20091227115154.1347:compute_trial_derivatives
double vpic__physics__harmonic_oscillator__compute_trial_derivatives(
    int number_of_particles, int number_of_dimensions,
    double trial_coefficients[number_of_dimensions],
    double particle_positions[number_of_particles][number_of_dimensions],
    double gradient_of_log_trial_fn[number_of_particles][number_of_dimensions]
){
    double laplacian_of_log_trial_fn;
    __vpif__physics__harmonic_oscillator_MOD_compute_trial_derivatives(
        &number_of_particles, &number_of_dimensions,
        trial_coefficients,
        particle_positions,
        gradient_of_log_trial_fn, &laplacian_of_log_trial_fn
    );
    return laplacian_of_log_trial_fn;
}
//@-node:gcross.20091227115154.1347:compute_trial_derivatives
//@-others
//@-node:gcross.20091227115154.1344:@thin harmonic_oscillator.c
//@-leo
