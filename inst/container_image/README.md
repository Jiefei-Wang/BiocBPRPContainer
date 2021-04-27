The doRedis worker image accepts the following environment variable on startup:

1. workerNum: The number of R workers in the container
2. sshPubKey: The ssh public key
3. queueName: the queue name used by the worker to retrive jobs from the server
4. serverIp: The IP of the redis server(default: localhost)
5. serverPort: The port used by the redis server to accept the incoming connection(default: 6379)
6. serverPassword: The password of the redis server
7. serverTimeout: The max wait time in seconds until the Redis connection fails(default: 60s)
8. sysPackages: The additional system packages(URl encoded) that will be installed before running the workers, each package is separated by a commas
9. RPackages: The additional R packages(URl encoded) that will be installed before running the workers, each package is separated by a commas


To build
```
docker build -t redis_param_image .
```
To run the worker container
```
docker run -it --env queueName=jobs --env serverPassword=123456 --env workerNum=2 redis_param_image 
```
To run the worker container and also install the R package `BiocParallel`
```
docker run -it --env queueName=jobs --env serverPassword=123456 --env workerNum=2 --env RPackages=BiocParallel redis_param_image 
```

## Note
If you want to test the server and the worker on the same machine, you have to set the network mode to "host" to allow the communication between the server and worker. For example
```
docker run -it --env queueName=jobs --env serverIp=localhost --env serverPort=6666 --env serverPassword=123456 --env workerNum=2 --network="host" redis_param_image
```