// Include helpers Stan Functions 
functions {
  #include ../lib/stan_utils.stanfunctions
}

// Data Input Layer 
data{
    // Settings and Indexing 
    int <lower = 1> N;
    int <lower = 0, upper =1> prior_only;
    int <lower = 2> N_neighbourhoods;
    array[N] int neighbourhoods_id;

    // 2D HSGP Settings 
    int <lower = 1> M1;
    int <lower = 1> M2;
    vector[N] x_km;
    vector[N] y_km;
    real <lower = 0> c;
    
    // Covariates 
    array[N] int <lower = 0, upper = 1> super_host;    
    
    // Outcome 
    vector[N] log_price;
}
// Data Transformations 
transformed data {
   // HSGP 2D Transformations ==============
   int M = M1 * M2;
   real Lx = c * max(abs(x_km)) + 1e-6;
   real Ly = c * max(abs(y_km)) + 1e-6;

   matrix[N, M1] Px = phi_1d(x_km, Lx, M1);
   matrix[N, M2] Py = phi_1d(y_km, Ly, M2);

   // Tensor product Basis 
   matrix[N, M] Phi;
   vector[N] lambda;

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
    // Random Intercept Parameters
    real alpha_log_price;
    real <lower = 0.001> sd_neighbourhoods_log_price; 
    vector[N_neighbourhoods] z_alpha;

    // Random Super Host Slope Parameters 
    real alpha_super_host_eff;
    real sd_neighbourhoods_super_host;
    vector[N_neighbourhoods] z_super_host;

    // HSGP 2D Paramters 
    real <lower = 0> alpha_gp;
    real <lower = 0> rho_gp;
    vector[M] z_gp;

    // Observation Variations 
    real <lower = 0.001> sd_obs;
}
// Transform Parameters 
transformed parameters {
   // Recover the Random Effects 
   vector[N_neighbourhoods] alpha_j;
   vector[N_neighbourhoods] beta_j;
   alpha_j = alpha_log_price + sd_neighbourhoods_log_price * z_alpha;
   beta_j  = alpha_super_host_eff + sd_neighbourhoods_super_host * z_super_host;

   // Recover HSGP 2D 
   vector[N] f_location;
   {
    vector[M] sqrt_spd = alpha_gp * sqrt(2 * pi()) * rho_gp * exp(-0.25 * square(rho_gp) * lambda);
    f_location = Phi * (sqrt_spd .* z_gp);
   }
}
// Model 
model{
    // Random Intercept Priors
    alpha_log_price ~ normal(log(30), 0.3);
    sd_neighbourhoods_log_price ~ exponential(1); 
    z_alpha ~ std_normal();

    // Random Super Host Slope Priors 
    alpha_super_host_eff ~ normal(0, 1);
    sd_neighbourhoods_super_host ~ exponential(1);
    z_super_host ~ std_normal();

    // HSGP 2D Priors 
    alpha_gp ~ normal(0, 1);
    rho_gp ~ inv_gamma(5, 5);
    z_gp ~ std_normal();

    // Observation Variations Prior 
    sd_obs ~ exponential(1);

    // Model Likelihood 
    if(prior_only == 0){
        vector[N] mu;
        for(i in 1:N){
            mu[i] = alpha_j[neighbourhoods_id[i]] 
                    + beta_j[neighbourhoods_id[i]] * super_host[i]
                    + f_location[i];
        }
    // Sample log_price from Normal Distribution 
    log_price ~ normal(mu, sd_obs);
    }
}
// Minimal Generated Quantities 
generated quantities {
   
}