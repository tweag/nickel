# test = 'pass'

# An illustrative (thus incomplete and maybe incorrect) contract example for a
# Kubernetes configuration.
# Schema and example derived from
# https://github.com/kubernetes/examples/blob/master/guestbook-go/guestbook-controller.json.
#
# This example is illustrative. If you actually want to use Nickel with
# Kubernetes, consider using the auto-generated contracts from
# https://github.com/tweag/nickel-kubernetes/ instead
let Port
  | doc "A contract for a port number"
  =
    std.contract.from_predicate (fun value =>
      std.is_number value
      && (value % 1 == 0)
      && (value >= 0)
      && (value <= 65535)
    )
in

let PortElt
  | doc "A contract for a port element of a Kubernetes configuration"
  = {
    name | String,
    containerPort | Port,
  }
in

let Container = {
  name | String,
  image | String,
  ports | Array PortElt,
}
in

let KubernetesConfig = {
  kind
    | [| 'ReplicationController, 'ReplicaSet, 'Pod |]
    | doc "The kind of the element being configured."
    | default
    = 'Pod,

  apiVersion | String,

  metadata
    | {
      name | String,
      labels.app | String,
    },

  spec
    | {
      replicas
        | std.number.PosNat
        | doc "The number of replicas"
        | default
        = 1,

      selector.matchLabels.app | String,

      template
        | {
          metadata.labels.app | String,
          spec.containers | Array Container,
        },
    },
}
in

let name = "myApp" in
let metadata = {
  include name,
  labels.app = name,
}
in

{
  kind = 'ReplicationController,
  apiVersion = "1.1.0",
  include metadata,
  spec = {
    replicas = 3,
    selector = {
      app = { include name },
      matchLabels.app = name,
    },
    template = {
      include metadata,
      spec = {
        containers = [
          {
            include name,
            image = "k8s.gcr.io/%{name}:v3",
            ports = [
              {
                name = "http-server",
                containerPort = 80,
              }
            ]
          }
        ]
      }
    }
  }
} | KubernetesConfig
