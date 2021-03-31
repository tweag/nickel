// An (incomplete and probably wrong) contract example for a Kubernetes
// configuration element.

// There's a similar contract coming to the `nums` stdlib, but for now, we
// redefine it. Similarly, in the future, common contracts such as `Port` or
// `Url` will be provided by libraries.
let PosNat | doc "A contract for a positive natural number" =
  fun label value =>
    if builtins.isNum value && value % 1 == 0 && value > 0 then
        value
      else
        contracts.blame label in

let Port | doc "A contract for a port number" =
  fun label value =>
    if builtins.isNum value &&
      value % 1 == 0 &&
      value >= 0 &&
      value <= 65535
    then
        value
      else
        contracts.blame label in

let PortElt
  | doc "A contract for a port element of a Kubernetes configuration"
  = {
  name | Str;
  containerPort | #Port; 
} in

let Container = {
  name | Str;
  image | Str;
  ports | List #PortElt;
} in

let KubernetesConfig = {
  kind | <ReplicationController, ReplicaSet, Pod>
       | doc "The kind of the element being configured."
       | default = `Pod;

  apiVersion | Str;

  metadata = {
    name | Str;
    labels.app | Str;
  };

  spec = {
    replicas | #PosNat
             | doc "The number of replicas"
             | default = 1;

    selector.matchLabels.app | Str;

    template = {
      metadata.labels.app | Str;
      spec.containers | List #Container;
    };
  };
} in

let name_ = "myApp" in
let metadata_ = {
    name = name_;
    labels.app = name_;
  } in
({
  kind = `ReplicationController;
  apiVersion = "1.1.0";
  metadata = metadata_;
  spec = {
    replicas = 3;
    selector = {
      app.name = name_;
      matchLabels.app = name_;
    };
    template = {
       metadata = metadata_;
       spec = {
          containers = [
            {
              name = name_;
              image = "k8s.gcr.io/#{name_}:v3";
              ports = [
                {
                  name = "http-server";
                  containerPort = 80;
                }
              ]
            }
          ]
        }
    }
  }
}
  | #KubernetesConfig)
