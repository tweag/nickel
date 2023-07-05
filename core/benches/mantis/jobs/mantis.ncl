let types = import "../schemas/nomad/types.ncl" in
let promtail = import "../tasks/promtail.ncl" in
let Telegraf = import "../tasks/telegraf.ncl" in
let lib = import "../lib.ncl" in

# Temporary dummy value
let baseTags = [""] in
let Params = {
  count
    | std.number.Nat
    | default
    = 5,
  role | [| 'passive, 'miner, 'backup |],
  datacenters | Dyn,
  job | Dyn,
  namespace | String,
  logLevel | String,
  mantisRev | String,
  fqdn | String,
  loggers | { _ : String },
  network
    | lib.contracts.OneOf ["testnet-internal-nomad", "etc"]
    | default
    = "testnet-internal-nomad",
  networkConfig | String,

  fastSync
    | Bool
    | default
    = false,
  reschedule | { .. }
  #{_ : lib.contracts.PseudoOr [
  #         lib.contracts.OrableFromPred builtin.is_string,
  #         lib.contracts.OrableFromPred builtin.is_number,
  #         lib.contracts.OrableFromPred builtin.is_bool,
  #         {
  #           pred = builtin.is_record,
  #           contract = {
  #             attempts = 0,
  #             unlimited = true,
  #           },
  #         }
  #       ]
  # },
}
in
fun params =>
  let params | Params = params in
  types.stanza.job
  & {
    namespace = params.namespace,
    datacenters = params.datacenters,
  }
  & (
    if params.network == "etc" then
      {
        type = 'batch,
        periodic = {
          prohibit_overlap = true,
          cron = "@daily",
          time_zone = 'UTC,
        }
      }
    else
      {}
  )
  & (
    if params.network != "etc" then
      {
        type = 'service,

        update = {
          max_parallel = 1,
          health_check = 'checks,
          min_healthy_time = "1m",
          # Give enough time for the DAG generation
          healthy_deadline = "15m",
          progress_deadline = "30m",
          auto_revert = false,
          auto_promote = false,
          canary = 0,
          stagger = "20m",
        },
      }
    else
      {}
  )
  & {
    group.mantis =
      {
        count = params.count,
        network = {
          mode = 'host,
          port = {
            discovery = {},
            metrics = {},
            rpc = {},
            server = {},
          }
        },

        ephemeral_disk = {
          size = 10 * 1000,
          migrate = true,
          sticky = true,
        }
      }
      & (
        if params.network == "etc" then
          {
            restart = {
              interval = "1m",
              attempts = 0,
              delay = "1m",
              mode = "fail",
            }
          }
        else
          {}
      )
      & {
        reschedule = params.reschedule,

        task.telegraf | Telegraf
          = {
            #infinte rec?
            # #namespace = namespace,
            name = "%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}",
            prometheusPort = "metrics",
          },

        # Do we really need this?
        #task.mantis | tasks.#Mantis = {
        #  #namespace:     namespace
        #  #mantisRev:     ref.mantisRev
        #  #role:          ref.role
        #  #logLevel:      ref.logLevel
        #  #networkConfig: ref.networkConfig
        #  #loggers:       ref.loggers
        #  #network:       ref.network
        #  #fastSync:      ref.fastSync
        #}

        task.promtail = promtail,

        #same?
        # #baseTags: [namespace, #role, "mantis-${NOMAD_ALLOC_INDEX}"]
      }
      & (
        if params.role == 'passive then
          {
            #TODO: dependent if
            service."%{params.namespace}-mantis-%{std.string.from_enum params.role}-rpc" = {
              address_mode = 'host,
              port = "rpc",
              tags =
                [
                  "rpc",
                  "ingress",
                  "traefik.enable=true",
                  "traefik.http.routers.%{params.namespace}-mantis-%{std.string.from_enum
params.role}.rule=Host(`%{params.namespace}-%{std.string.from_enum params.role}.%{params.fqdn}`)",
                  "traefik.http.routers.%{params.namespace}-mantis-%{std.string.from_enum params.role}.entrypoints=https",
                  "traefik.http.routers.%{params.namespace}-mantis-%{std.string.from_enum params.role}.tls=true",
                ]
                @ baseTags,
            }
          }
        else
          {}
      )
      & {
        #TODO: dependent if
        #For now, redefining namespace
        service."%{params.namespace}-mantis-%{std.string.from_enum params.role}-rpc" = {
          check.rpc =
            {
              address_mode = 'host,
              interval = "10s",
              port = "rpc",
              timeout = "3s",
              type = 'http,
              path = "/healthcheck",
            }
            & (
              if params.network != "etc" then
                {
                  check_restart = {
                    limit = 5,
                    grace = "10m",
                  }
                }
              else
                {}
            ),
        }
      }
      & (
        if params.role == 'miner then
          {
            # TODO: dependent if
            service."%{params.namespace}-mantis-%{std.string.from_enum params.role}-rpc" = {
              address_mode = 'host,
              port = "rpc",
              tags = ["rpc"] @ baseTags,
            }
          }
        else
          {}
      )
      & {
        service = {
          "%{params.namespace}-mantis-%{std.string.from_enum params.role}-prometheus" = {
            address_mode = 'host,
            port = "metrics",
            tags = ["prometheus"] @ baseTags,
          },

          "%{params.namespace}-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}" = {
            address_mode = 'host,
            port = "rpc",
            tags =
              [
                "rpc",
                "ingress",
                "traefik.enable=true",
                "traefik.http.routers.%{params.namespace}-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}.rule=Host(`%{params.namespace}-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}.%{params.fqdn}`)",
                "traefik.http.routers.%{params.namespace}-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}.entrypoints=https",
                "traefik.http.routers.%{params.namespace}-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}.tls=true",
              ]
              @ baseTags,
          },

          "%{params.namespace}-mantis-%{std.string.from_enum params.role}-discovery-${NOMAD_ALLOC_INDEX}" =
            {
              port = "discovery",
            }
            & (
              if params.role == 'miner then
                {
                  tags =
                    [
                      "ingress",
                      "discovery",
                      "traefik.enable=true",
                      # TODO: Dependent if (refer to namespace)
                      "traefik.tcp.routers.%{params.namespace}-discovery-${NOMAD_ALLOC_INDEX}.rule=HostSNI(`*`)",
                      "traefik.tcp.routers.%{params.namespace}-discovery-${NOMAD_ALLOC_INDEX}.entrypoints=%{params.namespace}-discovery-${NOMAD_ALLOC_INDEX}",
                    ]
                    @ baseTags
                }
              else
                {}
            )
            & (
              if params.role == 'passive then
                {
                  tags = ["discovery"] @ baseTags
                }
              else
                {}
            )
            & {
              meta = {
                Name = "mantis-${NOMAD_ALLOC_INDEX}",
                PublicIp = "${attr.unique.platform.aws.public-ipv4}",
              }
            },

          "%{params.namespace}-mantis-%{std.string.from_enum params.role}-server-${NOMAD_ALLOC_INDEX}" =
            {
              address_mode = 'host,
              port = "server",
            }
            & (
              if params.role == 'miner then
                {
                  tags =
                    [
                      "ingress",
                      "server",
                      "traefik.enable=true",
                      "traefik.tcp.routers.%{params.namespace}-server-${NOMAD_ALLOC_INDEX}.rule=HostSNI(`*`)",
                      "traefik.tcp.routers.%{params.namespace}-server-${NOMAD_ALLOC_INDEX}.entrypoints=%{params.namespace}-server-${NOMAD_ALLOC_INDEX}",
                    ]
                    @ baseTags,
                }
              else
                {}
            )
            & (
              if params.role == 'passive then
                {
                  tags = ["server"] @ baseTags,
                }
              else
                {}
            )
            & {
              meta = {
                Name = "mantis-%{std.string.from_enum params.role}-${NOMAD_ALLOC_INDEX}",
                PublicIp = "${attr.unique.platform.aws.public-ipv4}",
              },

              check.server =
                {
                  address_mode = 'host,
                  interval = "10s",
                  port = "server",
                  timeout = "3s",
                  type = 'tcp,
                }
                & (
                  if params.network != "etc" then
                    {
                      check_restart = {
                        limit = 5,
                        grace = "10m",
                      }
                    }
                  else
                    {}
                ),
            },

          "%{params.namespace}-mantis-%{std.string.from_enum params.role}-server" = {
            address_mode = 'host,
            port = "server",
            tags = ["ingress", "server"] @ baseTags,
            meta = {
              Name = "mantis-${NOMAD_ALLOC_INDEX}",
              PublicIp = "${attr.unique.platform.aws.public-ipv4}",
            }
          },

          "%{params.namespace}-mantis-%{std.string.from_enum params.role}" = {
            address_mode = 'host,
            port = "server",
            tags = ["server"] @ baseTags,
            meta = {
              Name = "mantis-${NOMAD_ALLOC_INDEX}",
              PublicIp = "${attr.unique.platform.aws.public-ipv4}",
            },
          },
        }
      },
  }
