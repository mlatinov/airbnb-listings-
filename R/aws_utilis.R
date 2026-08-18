
#### Function to Launch EC2 Instance From AMI ####
launch_stan_ec2_instance <- function(){
  ec2 <- paws.compute::ec2()

  # Launch EC2 Instance 
  launch <- ec2$run_instances(
    ImageId      = "ami-0daaca8c301de91da",
    InstanceType = "c7i.2xlarge",
    MinCount = 1, 
    MaxCount = 1,
    KeyName        = "stan-test-key-2",
    SecurityGroupIds = list("sg-00e41c33fb88cd266"),
    IamInstanceProfile = list(Name = "stan-ec2-s3"),
    BlockDeviceMappings = list(
      list(
        DeviceName = "/dev/sda1",
        Ebs = list(VolumeSize = 30)
      )
    )
  )
  instance_id <- launch$Instances[[1]]$InstanceId
  cat("Launched instance:", instance_id, "\n")
  instance_id
}

#### Function to Check the EC2 Running Status ####
get_instance_ip <- function(instance_id) {
  ec2 <- paws.compute::ec2()
  result <- ec2$describe_instances(InstanceIds = list(instance_id))
  instance <- result$Reservations[[1]]$Instances[[1]]
  list(
    state = instance$State$Name,
    public_ip = instance$PublicIpAddress
  )
}

#### Function to Terminate a Runing EC2 Instance ####
terminate_stan_ec2_instance <- function(instance_id) {
  ec2 <- paws.compute::ec2()
  # Terminate the EC2 given the instance ID from the launch_stan_ec2_instance()
  ec2$terminate_instances(InstanceIds = list(instance_id))
  cat("Terminated instance:", instance_id, "\n")
}

#### Function to push a local data into S3 ####
push_data_to_s3 <- function(bucket = "stan-airbnb-s3",key = "raw-data/listings.csv", path_to_data = "data/listings.csv"){
  s3 <- paws.storage::s3()
  # Store the data in S3
  s3$put_object(
    Bucket = bucket,
    Key    = key,
    Body = readBin(path_to_data, "raw", file.info(path_to_data)$size)
  )
}

#### Function to get back the data from S3 ####
get_data_from_s3 <- function(local_path_to_data = "data/listings.csv",bucket = "stan-airbnb-s3", key = "raw-data/listings.csv"){
  ## Check if the data file is not already present in the listed path ##
  if (!file.exists(local_path)) {
    # Create a temp directory for the data 
    dir.create("data", showWarnings = FALSE)
    # Get the data back 
    s3  <- paws.storage::s3()
    obj <- s3$get_object(Bucket = bucket, Key = key)
    writeBin(obj$Body, local_path)
  }
  local_path_to_data
}

#### Function to remotly launch the pipeline ####
run_remote_pipeline <- function(ip, key_path = "stan-test-key-2.pem", repo_dir = "airbnb-listings-") {
  cmd <- sprintf(
    "ssh -i %s ubuntu@%s 'cd %s && git pull && tmux new -d -s stanrun \"Rscript -e targets::tar_make()\"'",
    key_path, ip, repo_dir
  )
  system(cmd)
  cat("Pipeline started in remote tmux session 'stanrun'. SSH in and `tmux attach -t stanrun` to check progress.\n")
}