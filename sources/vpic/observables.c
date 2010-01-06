//@+leo-ver=4-thin
//@+node:gcross.20100105133218.1368:@thin observables.c
//@@language C

//@+others
//@+node:gcross.20100105133218.1373:compute_energy
double vpic__observables__compute_energy(
    int number_of_particles, int number_of_dimensions,
    double hbar_over_2m,
    double potential,
    double gradient_of_log_trial_fn[number_of_particles][number_of_dimensions],
    double laplacian_of_log_trial_fn
) {
    double energy;
    __vpif__observables_MOD_compute_energy_subroutine(
        &number_of_particles, &number_of_dimensions,
        &hbar_over_2m,
        &potential,
        gradient_of_log_trial_fn,
        &laplacian_of_log_trial_fn,
        &energy
    );
    return energy;
}
//@-node:gcross.20100105133218.1373:compute_energy
//@-others
//@-node:gcross.20100105133218.1368:@thin observables.c
//@-leo
