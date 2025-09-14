###/file.ncl
let ReplicationController = import "../imported/replicationcontroller-v1.ncl" in

{
  config = {
    name = "redis-master",
    labels = { app = "redis", role = "master" },
  },
  kubernetes | ReplicationController = {
    apiVersion = "v1",
    kind = "ReplicationController",
    metadata = { labels = config.labels, name = config.name },
    spec = {
      replicas = 1,
      selector = config.labels,
      template = {
        metadata = { labels = config.labels },
        spec.containers = [
          {
            image = "registry.k8s.io/redis:e2e",
            name = config.name,
            ports = [
              {
                # the error is here: containerPort is a string, but it should be
                # an integer
                containerPort = "200",
                name = config.name,
              }
            ],
          }
        ],
      },
    },
  }
}
