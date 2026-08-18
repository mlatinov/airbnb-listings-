// Include helpers Stan Functions 
functions {
  #include "../../lib/stan_utils.stanfunctions"
}

// Data Input Layer 
data{
    // Settings and Indexing 
    int <lower = 1> N;
    int <lower = 0, upper =1> prior_only;
    int <lower = 2> N_hoods;
    int <lower = 2> N_hood_groups;
    array[N] int    hoods_id;
    array[N] int    hood_groups_id;
    array[N] int <lower = 0, upper = 4> room_type_id;

    // 2D HSGP Settings 
    int <lower = 1> M1;
    int <lower = 1> M2;
    vector[N] x_km;
    vector[N] y_km;
    real <lower = 0> c;
    
    // Covariates 
    array[N] int <lower = 0, upper = 1  >   super_host;    
    array[N] int <lower = 0, upper = 16 >   accommodates;
    array[N] int <lower = 0>  guest_lead_months;
    array[N] int <lower = 0>  minimum_nights;
    
    // Outcome 
    vector[N] log_price;
}
// Data Transformations 
transformed data {
    // Muiltithreading Slicer 
    array[N] int slicer = linspaced_int_array(N, 1, N);

    // Z_score the non indicator Numerical Covariates 
    vector[N] accommodates_z      = zscore(to_vector(accommodates));
    vector[N] guest_lead_months_z = zscore(to_vector(guest_lead_months));
    vector[N] minimum_nights_z    = zscore(to_vector(minimum_nights));

    // HSGP 2D Transformations 
    int M = M1 * M2;
    real Lx = c * max(abs(x_km)) + 1e-6;
    real Ly = c * max(abs(y_km)) + 1e-6;
    matrix[N, M1] Px = phi_1d(x_km, Lx, M1);
    matrix[N, M2] Py = phi_1d(y_km, Ly, M2);

    // Tensor product Basis 
    matrix[N, M] Phi;
    vector[M] lambda;
    {
        int m = 1;
        for(j1 in 1:M1){
            for(j2 in 1:M2){
                Phi[, m] = Px[, j1] .* Py[, j2];
                lambda[m] = square(j1 * pi() / (2*Lx)) + square(j2 * pi() / (2*Ly));
                m += 1;
            }
        }
   }
}
// Model Parameters 
parameters{
    // Random Intercept Hood Parameters
    real                 alpha_bar;
    real <lower = 0.001> sd_hood;
    vector[N_hoods]      z_hood;
    
    // Random Intercept Hoods Groups Parameters
    real <lower = 0.001>   sd_hood_group; 
    vector[N_hood_groups]  z_hood_group;

    // Random Super Host Slope Parameters 
    real                 super_host_bar;
    real <lower = 0.001> sd_super_host;
    vector[N_hoods]      z_super_host;

    // Random Accommodates Slope Paramters 
    real                 accommodates_bar;
    real <lower = 0.001> sd_accommodates;
    vector[N_hoods]      z_accommodates;

    // HSGP 2D Parameters 
    real <lower = 0>             alpha_gp;
    real <lower = 0, upper = 30> rho_gp;
    vector[M]                    z_gp;

    // Fixed effects 
    real                  beta_guest_lead;
    real                  beta_minimum_nights;
    sum_to_zero_vector[4] room_effect;

    // Observation Variations 
    real <lower = 0.001> sd_obs;
}
// Transform Parameters 
transformed parameters {

    // Calculate Spectral Density 
    vector[M] sqrt_spd = alpha_gp * sqrt(2 * pi()) * rho_gp * exp(-0.25 * square(rho_gp) * lambda);
    
    // Partially Computed Linear Predictor without function of location  
    vector[N] mu;
    {   // Recover the parameters one by one 
        vector[N_hoods]       hood_effect          = sd_hood          * z_hood;
        vector[N_hoods]       super_host_effect    = super_host_bar   + sd_super_host   * z_super_host;
        vector[N_hoods]       accommodates_effect  = accommodates_bar + sd_accommodates * z_accommodates;
        vector[N_hood_groups] hood_group_effect    = sd_hood_group    * z_hood_group;

        // Compute mu 
        mu = 
            alpha_bar + hood_effect[hoods_id]  + hood_group_effect[hood_groups_id]
            + super_host_effect[hoods_id]     .* to_vector(super_host)
            + accommodates_effect[hoods_id]   .* accommodates_z
            + beta_guest_lead                  * guest_lead_months_z
            + beta_minimum_nights              * minimum_nights_z
            + room_effect[room_type_id]; 
    }
}
// Model 
model{
    // Random Intercept Hood Parameters
    alpha_bar ~ normal(log(240), 1);
    sd_hood   ~ exponential(1);
    z_hood    ~ std_normal();
    
    // Random Intercept Hoods Groups Parameters
    sd_hood_group ~ exponential(1); 
    z_hood_group  ~ std_normal();

    // Random Super Host Slope Parameters 
    super_host_bar ~ normal(0, 1);
    sd_super_host  ~ exponential(1);
    z_super_host   ~ std_normal();

    // Random Accommodates Slope Paramters 
    accommodates_bar ~ normal(0, 1);
    sd_accommodates  ~ exponential(1);
    z_accommodates   ~ std_normal();
    
    // Fixed effects 
    beta_guest_lead     ~ normal(0, 1);
    beta_minimum_nights ~ normal(0, 1);
    room_effect         ~ normal(0, 1);

    // HSGP 2D Priors
    alpha_gp ~ normal(0, 1);
    rho_gp   ~ inv_gamma(5, 5);
    z_gp     ~ std_normal();

    // Observation Variations 
    sd_obs ~ exponential(1);

    // Model Likelihood 
    if(prior_only == 0){
        profile("Likelihood"){
        // Normal Distribution Likelihood  
       target += reduce_sum(multithreaded_normal, slicer, 1, Phi, mu, log_price, sd_obs, z_gp, sqrt_spd);
    }}
}
// Minimal Generated Quantities 
generated quantities {
    vector[N] log_lik;
    vector[N] log_price_rep;

    for (i in 1:N) {
        log_lik[i]       = normal_lpdf(log_price[i] | mu[i], sd_obs);
        log_price_rep[i] = normal_rng(mu[i], sd_obs);
    }
}
