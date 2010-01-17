//@+leo-ver=4-thin
//@+node:gcross.20100106124611.1988:@thin second_order.c
//@@language C

//@+others
//@+node:gcross.20100106124611.2006:initialize_weights
double vpic__greens_function__second_order__initialize_weights(
    int number_of_slices,
    double weights[number_of_slices]
) {__vpif__greens_function__second_order_MOD_initialize_weights(
    &number_of_slices,
    weights
);}
//@-node:gcross.20100106124611.2006:initialize_weights
//@+node:gcross.20100106124611.2010:compute_log_greens_function
double vpic__greens_function__second_order__compute_log_greens_function(
    int number_of_slices,
    double slice_time_interval,
    double weights[number_of_slices],
    double potential[number_of_slices]
) {
    double log_gfn;
    __vpif__greens_function__second_order_MOD_compute_log_greens_function_(
        &number_of_slices,
        &slice_time_interval,
        weights,
        potential,
        &log_gfn
    );
    return log_gfn;
}
//@-node:gcross.20100106124611.2010:compute_log_greens_function
//@-others
//@-node:gcross.20100106124611.1988:@thin second_order.c
//@-leo
