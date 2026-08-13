// Include helpers Stan Functions 
functions {
  #include ../lib/stan_utils.stanfunctions
}

// Data Input Layer 
data{
    // Settings and Indexing 
    int <lower = 1> N;
    int <lower = 0, upper =1> prior_only;
    int <lower = 2> N_hoods;
    int <lower = 2> N_hood_groups;
    array[N] int  hoods_id;
    array[N] int  hood_groups_id;

    // 2D HSGP Settings 
    int <lower = 1> M1;
    int <lower = 1> M2;
    vector[N] x_km;
    vector[N] y_km;
    real <lower = 0> c;
    
    // Covariates 
    array[N] int <lower = 0, upper = 1  >   super_host;    
    array[N] int <lower = 0, upper = 16 >   accommodates;
    
    // Outcome 
    vector[N] log_price;
}
// Data Transformations 
transformed data {
    // Z_score the non indicator Numerical Covariates 
    vector[N] accommodates_z = zscore(to_vector(accommodates));

    // HSGP 2D Transformations ==============
    int M = M1 * M2;
    real Lx = c * max(abs(x_km)) + 1e-6;
    real Ly = c * max(abs(y_km)) + 1e-6;
    
    matrix[N, M1] Px;
    matrix[N, M2] Py;
    
    profile("Phi 1D "){
    Px = phi_1d(x_km, Lx, M1);
    Py = phi_1d(y_km, Ly, M2);}

    // Tensor product Basis 
    matrix[N, M] Phi;
    vector[M] lambda;
    profile("HSGP Tensor Product"){
    {
        int m = 1;
        for(j1 in 1:M1){
            for(j2 in 1:M2){
                Phi[, m] = Px[, j1] .* Py[, j2];
                lambda[m] = square(j1 * pi() / (2*Lx)) + square(j2 * pi() / (2*Ly));
                m += 1;
            }
        }
   }}
}
// Model Parameters 
parameters{
    // Random Correlated Intercept Hood Parameters
    real alpha_bar;

    // Random Correlated Super Host Slope Parameters 
    real super_host_bar;

    // Random Correlated Accommodates Slope Paramters 
    real accommodates_bar;

    // Correlation Between Hood Groups, Super Host and Accommodates specific paramters 
    vector <lower = 0.001>[3] sds;
    cholesky_factor_corr[3]   Lr;
    matrix[3, N_hoods]  z_matrix;

    // Random Intercept Hoods Groups Parameters
    real <lower = 0.001>   sd_hood_group; 
    vector[N_hood_groups]  z_hood_group;

    // HSGP 2D Parameters 
    real <lower = 0>             alpha_gp;
    real <lower = 0, upper = 30> rho_gp;
    vector[M]                    z_gp;

    // Observation Variations 
    real <lower = 0.001> sd_obs;
}
// Transform Parameters 
transformed parameters {
    // Recover the Correlation effect 
    matrix[3, N_hoods] omega = diag_pre_multiply(sds, Lr) * z_matrix;

    // Recover the Random Hood Group effect 
    vector[N_hood_groups] alpha_hood_group = sd_hood_group * z_hood_group;
    
    // Recover HSGP 2D 
    vector[N] f_location;
    profile("Spectral Density"){
    {
        // Spectral Density 
        vector[M] sqrt_spd = alpha_gp * sqrt(2 * pi()) * rho_gp * exp(-0.25 * square(rho_gp) * lambda);
        f_location = Phi * (sqrt_spd .* z_gp);
    }}
    
    // Linear Predictor 
    vector[N] mu;
    {   // Recover the correlated parameters one by one 
        vector[N] hoods               = alpha_bar        + to_vector(omega[1, hoods_id]);
        vector[N] super_host_effect   = super_host_bar   + to_vector(omega[2, hoods_id]) .* to_vector(super_host);
        vector[N] accommodates_effect = accommodates_bar + to_vector(omega[3, hoods_id]) .* accommodates_z;
        
        // Hood Group Intercept offect 
        vector[N] hood_group_offcet = alpha_hood_group[hood_groups_id];
        
        // Compute mu 
        mu = hoods + hood_group_offcet + super_host_effect + accommodates_effect + f_location;
    }
}
// Model 
model{
    // Random Correlated Intercept Hoods Prior
    alpha_bar ~ normal(log(240), 0.3);

    // Random Correlated Super Host Slope Prior 
    super_host_bar   ~ normal(0, 1);

    // Random Correlated Accommodates Slope Prior 
    accommodates_bar ~ normal(0, 1);

    // Correlation Between Hood Groups, Super Host and Accommodates specific Priors 
    sds ~ exponential(1);
    Lr  ~ lkj_corr_cholesky(2); 
    to_vector(z_matrix) ~ std_normal();

    // Random Intercept Hood Groups Priors
    sd_hood_group ~ exponential(1);
    z_hood_group  ~ std_normal();

    // HSGP 2D Priors
    alpha_gp ~ normal(0, 1);
    rho_gp   ~ inv_gamma(5, 5);
    z_gp     ~ std_normal();

    // Observation Variations 
    sd_obs ~ exponential(1);

    // Model Likelihood 
    if(prior_only == 0){
        // Sample log_price from Normal Distribution 
        profile("Likelihood"){
        log_price ~ normal(mu, sd_obs);
    }}
}
// Minimal Generated Quantities 
generated quantities {
    vector[N] log_lik;
    vector[N] log_price_rep;
    matrix[3, 3] Rho = multiply_lower_tri_self_transpose(Lr);
    
    for (i in 1:N) {
        log_lik[i]       = normal_lpdf(log_price[i] | mu[i], sd_obs);
        log_price_rep[i] = normal_rng(mu[i], sd_obs);
    }
}
