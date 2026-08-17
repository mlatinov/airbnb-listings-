
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

id <- launch_stan_ec2_instance()
get_instance_ip(id)
terminate_stan_ec2_instance(id)