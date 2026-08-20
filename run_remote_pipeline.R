source("R/aws_utilis.R")

# launch instance 
id <- launch_stan_ec2_instance()
ip <- get_instance_ip(id)$public_ip

# trigger remote run — pulls latest, runs tar_make() 
run_remote_pipeline(ip)

# Terminate
kill_finished_pipeline(ip,instance_id = id,check_min = 5)