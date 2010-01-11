//@+leo-ver=4-thin
//@+node:gcross.20100109140101.1545:@thin position.c
//@@language C

//@+others
//@+node:gcross.20100109140101.1546:bin_all_1d_integrated_slices
void vpic__histograms__position__bin_all_1d_integrated_slices(
    int number_of_particles, int number_of_dimensions,
    double particle_positions[number_of_particles][number_of_dimensions],
    int number_of_bins,
    double lower_bounds[number_of_dimensions], double upper_bounds[number_of_dimensions],
    int histogram[number_of_dimensions][number_of_bins]
) { __vpif__histograms__position_MOD_bin_all_1d_integrated_slices(
    &number_of_particles, &number_of_dimensions,
    particle_positions,
    &number_of_bins,
    lower_bounds, upper_bounds,
    histogram
);}
//@-node:gcross.20100109140101.1546:bin_all_1d_integrated_slices
//@-others
//@-node:gcross.20100109140101.1545:@thin position.c
//@-leo
