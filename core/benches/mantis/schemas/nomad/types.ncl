let lib = import "../../lib.ncl" in

# DUMMY
let time = {
  parseDurationSecond = fun arg => 0 | String,
}
in

# DUMMY
# let stanza = {} in
{
  json = {
    Job = {
      Namespace | String,
      Id | Name,
      Name | String,
      Type | [| 'service, 'system, 'batch |],
      Priority | std.number.Nat,
      Datacenters | std.array.NonEmpty,
      TaskGroups
        | Array TaskGroup
        | default
        = [],
      Affinities
        | Array Affinity
        | default
        = [],
      Constraints
        | Array Constraint
        | default
        = [],
      Spreads
        | Array Spread
        | default
        = [],
      ConsulToken | lib.contracts.Nullable String,
      VaultToken | lib.contracts.Nullable String,
      Vault
        | lib.contracts.Nullable json.Vault
        | default
        = null,
      Update
        | lib.contracts.Nullable json.Update
        | default
        = null,
      Migrate
        | lib.contracts.Nullable json.Migrate
        | default
        = null,
      Periodic
        | lib.contracts.Nullable json.Periodic
        | default
        = null,
    },

    Affinity = {
      LTarget | String,
      RTargety | String,
      Operand
        | (
          lib.contracts.OneOf
            [
              "regexp",
              "set_contains_all",
              "set_contains",
              "set_contains_any",
              "=",
              "==",
              "is",
              "!=",
              "not",
              ">",
              ">=",
              "<",
              "<=",
              "version"
            ]
        ),
      Weight
        | std.number.Nat
        | lib.contracts.NotEq 0
        | lib.contracts.GreaterEq
        - 100
        | lib.contracts.SmallerEq 100,
    },

    Constraint = {
      LTarget
        | lib.contracts.Nullable String
        | default
        = null,
      RTarget | String,
      Operand
        | (
          lib.contracts.OneOf
            [
              "regexp",
              "set_contains",
              "distinct_hosts",
              "distinct_property",
              "=",
              "==",
              "is",
              "!=",
              "not",
              ">",
              ">=",
              "<",
              "<="
            ]
        ),
    },

    Spread = {
      Attribute | String,
      Weight
        | lib.contracts.Nullable
          (
            lib.contracts.AllOf
              [
                std.number.Nat,
                lib.contracts.GreaterEq - 100,
                lib.contracts.LesserEq 100
              ]
          )
        | default
        = null,
      SpreadTarget
        | Array SpreadTargetElem
        | default
        = [],
    },

    SpreadTargetElem = {
      Value | String,
      Percent
        | lib.contracts.Nullable
          (
            lib.contracts.AllOf
              [
                std.number.PosNat,
                lib.contracts.LesserEq 100
              ]
          )
        | default
        = null,
    },

    RestartPolicy = {
      Attempts | std.number.Nat,
      Interval | std.number.Nat,
      Delay | std.number.Nat,
      Mode | (lib.contracts.OneOf ["delay", "fail"]),
    },

    Volume = {
      Name | String,
      Type | (lib.contracts.OneOf [ null, "host", "csi"]),
      Source | String,
      ReadOnly
        | Bool
        | default
        = false,
      MountOptions
        | lib.contracts.Nullable
          {
            FsType
              | lib.contracts.Nullable String
              | default
              = null,
            mountFlags
              | lib.contracts.Nullable String
              | default
              = null
          }
        | default
        = null,
    },

    ReschedulePolicy = {
      Attempts
        | lib.contracts.Nullable std.number.Nat
        | default
        = null,
      DelayFunction
        | lib.contracts.Nullable
          (
            lib.contracts.OneOf
              [
                "constant",
                "exponential",
                "fibonacci"
              ]
          )
        | default
        = null,
      Delay
        | lib.contracts.Nullable
          (
            lib.contracts.AllOf
              [
                std.number.Nat,
                lib.contracts.GreaterEq 5 * 1000
              ]
          ) # >=time.ParseDuration("5s")
        | default
        = null,
      Interval
        | lib.contracts.Nullable std.number.Nat
        | default
        = null,
      MaxDelay
        | lib.contracts.Nullable std.number.Nat
        | default
        = null,
      Unlimited
        | lib.contracts.Nullable Bool
        | default
        = null,
    },

    Migrate = {
      HealthCheck
        | [| 'checks, 'task_states |]
        | default
        = 'checks,
      HealthyDeadline
        | std.number.Nat
        | default
        = 500000000000,
      MaxParallel
        | std.number.Nat
        | default
        = 1,
      MinHealthyTime
        | std.number.Nat
        | default
        = 10000000000,
    },

    Periodic = {
      Enabled
        | Bool
        | default
        = false,
      TimeZone
        | String
        | default
        = "UTC",
      SpecType = "cron",
      Spec | String,
      ProhibitOverlap
        | Bool
        | default
        = false,
    },

    Update = {
      AutoPromote | Bool | default = false,
      AutoRevert | Bool | default = false,
      Canary | std.number.Nat | default = 0,
      HealthCheck
        | [| 'checks, 'task_states, 'manual |]
        | default
        = 'checks,
      HealthyDeadline | lib.contracts.Nullable std.number.Nat | default = null,
      MaxParallel | std.number.Nat | default = 1,
      MinHealthyTime | lib.contracts.Nullable std.number.Nat | default = null,
      ProgressDeadline | lib.contracts.Nullable std.number.Nat | default = null,
      Stagger | lib.contracts.Nullable std.number.Nat | default = null,
    },

    TaskGroup = {
      Affinities
        | Array Affinity
        | default
        = [],
      Constraints
        | Array Constraint
        | default
        = [],
      Spreads
        | Array Spread
        | default
        = [],
      Count
        | std.number.Nat
        | lib.contracts.Greater 0,
      # TODO: Meta  [string]  string
      Name | String,
      RestartPolicy
        | lib.contracts.Nullable json.RestartPolicy
        | default
        = null,
      Services
        | Array Service
        | default
        = [],
      ShutdownDelay | std.number.Nat | default = 0,
      Tasks
        | Array Task
        | default
        = [],
      # TODO Volumes: [string]:  #json.Volume
      ReschedulePolicy | json.ReschedulePolicy,
      EphemeralDisk
        | lib.contracts.Nullable
          {
            Migrate | Bool,
            SizeMB | std.number.Nat,
            Sticky | Bool,
          }
        | default
        = null,
      Migrate
        | lib.contracts.Nullable json.Migrate
        | default
        = null,
      Update
        | lib.contracts.Nullable json.Update
        | default
        = null,
      Networks
        | Array json.Network
        | default
        = [],
      StopAfterClientDisconnect
        | lib.contracts.Nullable std.number.Nat
        | default
        = null,
      Scaling = null,
      Vault
        | lib.contracts.Nullable json.Vault
        | default
        = null,
    },

    Port = {
      Label | String,
      Value | lib.contracts.Nullable std.number.Nat | default = null,
      # used for static ports
      To | lib.contracts.Nullable std.number.Nat | default = null,
      HostNetwork | String | default = "",
    },

    Network = {
      Mode | [| 'host, 'bridge |] | default = 'host,
      Device | String | default = "",
      CIDR | String | default = "",
      IP | String | default = "",
      DNS = null,
      ReservedPorts | lib.contracts.Nullable (Array json.Port) | default = null,
      DynamicPorts | lib.contracts.Nullable (Array json.Port) | default = null,
      MBits = null,
    },

    ServiceCheck = {
      AddressMode | [| 'alloc, 'driver, 'host |],
      Args | lib.contracts.Nullable (Array String) | default = null,
      CheckRestart | json.CheckRestart,
      Command | String | default = "",
      Expose = false,
      FailuresBeforeCritical | std.number.Nat | default = 0,
      Id | String | default = "",
      InitialStatus | String | default = "",
      Interval | std.number.Nat | default = 10000000000,
      Method | String | default = "",
      Name | String | default = "",
      Path | String | default = "",
      PortLabel | String,
      Protocol | String | default = "",
      SuccessBeforePassing | std.number.Nat | default = 0,
      TaskName | String | default = "",
      Timeout | std.number.Nat,
      TLSSkipVerify | Bool | default = false,
      Type | [| 'http, 'tcp, 'script, 'grpc |],
      Body | lib.contracts.Nullable String | default = null,
      # TODO  Header  [string]  [...string]
    },

    CheckRestart
      | lib.contracts.Nullable
        {
          Limit | std.number.Nat | default = 0,
          Grace | std.number.Nat | default = 10000000000,
          IgnoreWarnings | Bool | default = false,
        },

    Lifecycle = {
      Hook | [| 'prestart, 'poststart, 'poststop |],
      Sidecar | lib.contracts.Nullable Bool | default = null,
    },

    LogConfig
      | lib.contracts.Nullable
        {
          MaxFiles | std.number.PosNat,
          MaxFileSizeMB | std.number.PosNat,
        },

    Service = {
      Id | String | default = "",
      Name | String,
      Tags
        | Array String
        | default
        = [],
      CanaryTags
        | Array String
        | default
        = [],
      EnableTagOverride
        | Bool
        | default
        = false,
      PortLabel | String,
      AddressMode | [| 'alloc, 'auto, 'driver, 'host |],
      Checks
        | Array ServiceCheck
        | default
        = [],
      CheckRestart | json.CheckRestart,
      Connect = null,
      # TODO Meta: [string]: string
      TaskName | String | default = "",
    },

    Task = {
      Name | String,
      Driver | [| 'exec, 'docker, 'nspawn |],
      Config
        | stanza.taskConfig
        | { driver | Driver },
      Constraints
        | Array Constraint
        | default
        = [],
      Affinities
        | Array Affinity
        | default
        = [],
      # TODO: Env: [string]: string
      Services
        | Array Service
        | default
        = [],
      Resources = {
        CPU
          | std.number.Nat
          | lib.contracts.GreaterEq 100
          | default
          = 100,
        MemoryMB
          | std.number.Nat
          | lib.contracts.GreaterEq 32
          | default
          = 300,
        DiskMB
          | lib.contracts.Nullable std.number.Nat
          | default
          = null,
      },
      Meta = {},
      RestartPolicy | lib.contracts.Nullable json.RestartPolicy | default = null,
      ShutdownDelay | std.number.Nat | default = 0,
      User | String | default = "",
      Lifecycle | lib.contracts.Nullable json.Lifecycle | default = null,
      KillTimeout | lib.contracts.Nullable std.number.Nat | default = null,
      LogConfig | json.LogConfig,
      Artifacts
        | Array json.Artifact
        | default
        = [],
      Templates
        | Array json.Template
        | default
        = [],
      DispatchPayload = null,
      VolumeMounts
        | Array json.VolumeMount
        | default
        = [],
      Leader | Bool | default = false,
      KillSignal | String,
      ScalingPolicies = null,
      Vault | lib.contracts.Nullable json.Vault | default = null,
    },

    VolumeMount = {
      Destination | String,
      PropagationMode | String,
      ReadOnly | Bool,
      Volume | String,
    },

    Artifact = {
      GetterSource | String,
      # TODO GetterOptions: [string]: string
      # TODO GetterHeaders: [string]: string
      GetterMode | [| 'any, 'file, 'dir |] | default = 'any,
      RelativeDest | String,
    },

    Template = {
      SourcePath | String | default = "",
      DestPath | String,
      EmbeddedTmpl | String,
      ChangeMode | [| 'restart, 'noop, 'signal |] | default = 'restart,
      ChangeSignal | String | default = "",
      Splay | std.number.Nat | default = 5000000000,
      Perms | lib.contracts.MatchRegexp "^[0-7]{4}$" | default = "0644",
      LeftDelim | String,
      RightDelim | String,
      Envvars | Bool,
    },

    Vault = {
      ChangeMode | [| 'noop, 'restart, 'signal |] | default = 'restart,
      ChangeSignal | String | default = "",
      Env | Bool | default = true,
      Namespace | String | default = "",
      Policies | std.array.NonEmpty,
    }
  },

  Duration = lib.contracts.MatchRegexp "^[1-9]\\d*[hms]$",
  GitRevision = lib.contracts.MatchRegexp "^[a-f0-9]{40}$",
  Flake = lib.contracts.MatchRegexp "^(github|git\\+ssh|git):[0-9a-zA-Z_-]+/[0-9a-zA-Z_-]+",

  toJson =
    json.Job
    & {
      job = stanza.job,
      jobName | String,
      Name = jobName,
      Datacenters = job.datacenters,
      Namespace = job.namespace,
      Type = job.type,
      Priority = job.priority_,

      # TODO: cannot convert to merge yet because job.update is located in another
      # piece of the record.

      # if #job.update != null {
      #   let u = #job.update
      #   Update: {
      #     AutoPromote:      u.auto_promote
      #     AutoRevert:       u.auto_revert
      #     Canary:           u.canary
      #     HealthCheck:      u.health_check
      #     HealthyDeadline:  time.ParseDuration(u.healthy_deadline)
      #     MaxParallel:      u.max_parallel
      #     MinHealthyTime:   time.ParseDuration(u.min_healthy_time)
      #     ProgressDeadline: time.ParseDuration(u.progress_deadline)
      #     Stagger:          time.ParseDuration(u.stagger)
      #   }
      # }

      # TODO: Same.
      # if #job.migrate != null {
      #   let m = #job.migrate
      #   Migrate: {
      #     HealthCheck:     m.health_check
      #     HealthyDeadline: m.healthy_deadline
      #     MaxParallel:     m.max_parallel
      #     MinHealthyTime:  m.min_healthy_time
      #   }
      # }

      # TODO: Same.
      # if #job.periodic != null {
      #   let p = #job.periodic
      #   Periodic: {
      #     Enabled:         true
      #     TimeZone:        p.time_zone
      #     Spec:            p.cron
      #     ProhibitOverlap: p.prohibit_overlap
      #   }
      # }

      # TODO: Same.
      # if #job.vault != null {
      #   Vault: {
      #     ChangeMode:   #job.vault.change_mode
      #     ChangeSignal: #job.vault.change_signal
      #     Env:          #job.vault.env
      #     Namespace:    #job.vault.namespace
      #     Policies:     #job.vault.policies
      #   }
      # }

      Affinities =
        std.array.map
          (
            fun a =>
              {
                LTarget = a.attribute,
                RTarget = a.value,
                Operand = a.operator,
                Weight = a.weight
              }
          )
          job.affinities,

      Constraints =
        std.array.map
          (
            fun c =>
              {
                LTarget = c.attribute,
                RTarget = c.value,
                Operand = c.operator
              }
          )
          job.constraints,

      Spreads =
        std.array.map
          (
            fun s =>
              {
                Attribute = s.attribute,
                Weight = s.weight,
                SpreadTarget =
                  std.array.map
                    (
                      fun t =>
                        {
                          Value = t.value,
                          Percent = t.percent
                        }
                    )
                    s.target,
              }
          )
          job.spreads,

      TaskGroups =
        lib.records.mapToArray
          (
            fun tgName tg =>
              {
                Name = tgName,
                Count = tg.count,

                Affinities =
                  std.array.map
                    (
                      fun a =>
                        {
                          LTarget = a.attribute,
                          RTarget = a.value,
                          Operand = a.operator,
                          Weight = a.weight,
                        }
                    )
                    tg.affinities,

                Constraints =
                  std.array.map
                    (
                      fun c =>
                        {
                          LTarget = c.attribute,
                          RTarget = c.value,
                          Operand = c.operator,
                        }
                    )
                    tg.constraints,

                Spreads =
                  std.array.map
                    (
                      fun s =>
                        {
                          Attribute = s.attribute,
                          Weight = s.weight,
                          SpreadTarget =
                            std.array.map
                              (
                                fun t =>
                                  {
                                    Value = t.value,
                                    Percent = t.percent,
                                  }
                              )
                              s.target,
                        }
                    )
                    tg.spreads,
              }
              & (
                if tg.reschedule != null then
                  {
                    ReschedulePolicy =
                      {
                        Attempts = tg.reschedule.attempts,
                        DelayFunction = tg.reschedule.delay_function,
                        Unlimited = tg.reschedule.unlimited,
                      }
                      & (
                        if tg.reschedule.delay != null then
                          {
                            # test above was: != _|_. Not sure what is the difference? Should
                            # we test for the presence of the field?
                            Delay = time.ParseDuration tg.reschedule.delay
                          }
                        else
                          {}
                      )
                      & (
                        if tg.reschedule.interval != null then
                          {
                            # test above was: != _|_. Not sure what is the difference? Should
                            # we test for the presence of the field?
                            Interval = time.ParseDuration tg.reschedule.interval
                          }
                        else
                          {}
                      )
                      & (
                        if tg.reschedule.max_delay != null then
                          {
                            # test above was: != _|_. Not sure what is the difference? Should
                            # we test for the presence of the field?
                            MaxDelay = time.ParseDuration tg.reschedule.max_delay
                          }
                        else
                          {}
                      )
                  }
                else
                  {}
              )
              & (
                if tg.ephemeral_disk != null then
                  {
                    EphemeralDisk = {
                      SizeMB = tg.ephemeral_disk.size,
                      Migrate = tg.ephemeral_disk.migrate,
                      Sticky = tg.ephemeral_disk.sticky,
                    }
                  }
                else
                  {}
              )
              & (
                if tg.restart != null then
                  {
                    RestartPolicy = {
                      Interval = time.ParseDuration tg.restart.interval,
                      Attempts = tg.restart.attempts,
                      Delay = time.ParseDuration tg.restart.delay,
                      Mode = tg.restart.mode,
                    }
                  }
                else
                  {}
              )
              # only one network can be specified at group level, and we never use
              # deprecated task level ones.
              & (
                if tg.network != null then
                  let ports =
                    tg.network.port
                    |> lib.records.toArray
                    |> std.array.partition
                      tg.network.port
                      (
                        fun entry =>
                          entry.value.static != null
                      )
                  in
                  # would be better with destructuring
                  let mkPort = fun { key, value = { static, to, host_network }, .. } =>
                    {
                      Label = key,
                      Value = static,
                      To = to,
                      HostNetwork = host_network,
                    }
                  in
                  {
                    Networks = [
                      {
                        Mode = tg.network.mode,
                        ReservedPorts = std.array.map mkPort ports.right,
                        DynamicPorts = std.array.map mkPort ports.wrong,
                      }
                    ]
                  }
                else
                  {}
              )
              & {
                Services =
                  std.array.map
                    (
                      fun sName s =>
                        {
                          Name = sName,
                          TaskName = s.task,
                          Tags = s.tags,
                          AddressMode = s.address_mode,
                        }
                        & (
                          if s.check_restart != null then
                            {
                              CheckRestart = {
                                Limit = s.check_restart.limit,
                                Grace = time.ParseDuration (s.check_restart.grace),
                                IgnoreWarnings = s.check_restart.ignore_warnings,
                              }
                            }
                          else
                            {}
                        )
                        & {
                          Checks =
                            lib.records.mapToArray
                              (
                                fun cName c =>
                                  {
                                    AddressMode = c.address_mode,
                                    Type = c.type,
                                    PortLabel = c.port,
                                    Interval = time.ParseDuration (c.interval)
                                  }
                                  & (
                                    if c.type == "http" then
                                      {
                                        Path = c.path,
                                        Method = c.method,
                                        Protocol = c.protocol
                                      }
                                    else
                                      {}
                                  )
                                  & {
                                    Timeout = time.ParseDuration (c.timeout),
                                    SuccessBeforePassing = c.success_before_passing,
                                    FailuresBeforeCritical = c.failures_before_critical,
                                    TLSSkipVerify = c.tls_skip_verify,
                                    InitialStatus = c.initial_status,
                                    Header = c.header,
                                    Body = c.body,
                                  }
                                  & (
                                    if c.check_restart != null then
                                      {
                                        CheckRestart = {
                                          Limit = c.check_restart.limit,
                                          Grace = time.ParseDuration (c.check_restart.grace),
                                          IgnoreWarnings = c.check_restart.ignore_warnings,
                                        }
                                      }
                                    else
                                      {}
                                  )
                              )
                              s.check,

                          PortLabel = s.port,
                          Meta = s.meta,
                        }
                    )
                    tg.service,

                Tasks =
                  lib.records.mapToArray
                    (
                      fun tName t =>
                        {
                          Name = tName,
                          Driver = t.driver,
                          Config = t.config,
                          Env = t.env,
                          KillSignal = t.kill_signal,
                        }
                        & (
                          if t.kill_timeout != null then
                            {
                              KillTimeout = time.ParseDuration (t.kill_timeout)
                            }
                          else
                            {}
                        )
                        & {
                          Affinities =
                            std.array.map
                              (
                                fun { attribute, value, operator, weight, .. } =>
                                  {
                                    LTarget = attribute,
                                    RTarget = value,
                                    Operand = operator,
                                    Weight = weight,
                                  }
                              )
                              t.affinities,
                          # END AFFINITIES

                          Constraints =
                            lib.records.mapToArray
                              (
                                fun { attribute, value, operator } =>
                                  {
                                    LTarget = attribute,
                                    RTarget = value,
                                    Operand = operator,
                                  }
                              )
                              t.constraints,
                          # END CONSTRAINTS
                        }
                        & (
                          if t.logs != null then
                            {
                              LogConfig = {
                                MaxFiles = t.logs.max_files,
                                MaxFileSizeMB = t.logs.max_file_size,
                              }
                            }
                          else
                            {}
                        )
                        & (
                          if t.restart != null then
                            {
                              RestartPolicy = {
                                Interval = time.ParseDuration (t.restart.interval),
                                Attempts = t.restart.attempts,
                                Delay = time.ParseDuration (t.restart.delay),
                                Mode = t.restart.mode,
                              }
                            }
                          else
                            {}
                        )
                        & (
                          if t.lifecycle != null then
                            {
                              Lifecycle = {
                                Hook = t.lifecycle.hook,
                                Sidecar = t.lifecycle.sidecar,
                              }
                            }
                          else
                            {}
                        )
                        & {
                          Resources = {
                            CPU = t.resources.cpu,
                            MemoryMB = t.resources.memory,
                          },

                          Leader = t.leader,

                          Templates =
                            lib.records.mapToArray
                              (
                                fun tplName tpl =>
                                  {
                                    DestPath = tplName,
                                    EmbeddedTmpl = tpl.data,
                                    SourcePath = tpl.source,
                                    Envvars = tpl.env,
                                    ChangeMode = tpl.change_mode,
                                    ChangeSignal = tpl.change_signal,
                                    Perms = tpl.perms,
                                    LeftDelim = tpl.left_delimiter,
                                    RightDelim = tpl.right_delimiter,
                                  }
                              )
                              t.template,

                          Artifacts =
                            lib.records.mapToArray
                              (
                                fun artName art =>
                                  {
                                    GetterHeaders = art.headers,
                                    GetterMode = art.mode,
                                    GetterOptions = art.options,
                                    GetterSource = art.source,
                                    RelativeDest = artName,
                                  }
                              )
                              t.artifact,
                        }
                        & (
                          if t.vault != null then
                            {
                              Vault = {
                                ChangeMode = t.vault.change_mode,
                                ChangeSignal = t.vault.change_signal,
                                Env = t.vault.env,
                                Namespace = t.vault.namespace,
                                Policies = t.vault.policies,
                              }
                            }
                          else
                            {}
                        )
                        & {
                          VolumeMounts =
                            lib.records.mapToArray
                              (
                                fun volName vol =>
                                  {
                                    Destination = vol.destination,
                                    PropagationMode = "private",
                                    ReadOnly = vol.read_only,
                                    Volume = volName,
                                  }
                              )
                              t.volume_mount,
                        }
                    )
                    tg.task,
                # END TASKS
              }
              & (
                if tg.vault != null then
                  {
                    Vault = {
                      ChangeMode = tg.vault.change_mode,
                      ChangeSignal = tg.vault.change_signal,
                      Env = tg.vault.env,
                      Namespace = tg.vault.namespace,
                      Policies = tg.vault.policies,
                    }
                  }
                else
                  {}
              )
              & lib.records.mapToArray
                (
                  fun volName vol =>
                    {
                      Volumes."#{volName}" =
                        {
                          Name = volName,
                          Type = vol.type,
                          Source = vol.source,
                          ReadOnly = vol.read_only,
                        }
                        & (
                          if vol.type == "csi" then
                            {
                              MountOptions = {
                                FsType = vol.mount_options.fs_type,
                                mountFlags = vol.mount_options.mount_flags,
                              }
                            }
                          else
                            {}
                        )
                    }
                )
                tg.volume
          )
          job.group,
    },

  stanza = {
    job =
      let type_schema = [| 'batch, 'service, 'system |] in
      {
        datacenters | std.array.NonEmpty,
        namespace | String,
        type
          | type_schema
          | default
          = 'service,
        affinities
          | Array stanza.affinity
          | default
          = [],
        constraints
          | Array stanza.constraint
          | default
          = [],
        spreads
          | Array stanza.spread
          | default
          = [],
        group
          | {
            _ : stanza.group
            & {
              type | type_schema | default = 'service
            }
          },
        update
          | lib.contracts.Nullable stanza.update
          | default
          = null,
        vaultc
          | lib.contracts.Nullable stanza.vault
          | default
          = null,
        priority_
          | std.number.Nat
          | default
          = 50,
        periodic
          | lib.contracts.Nullable stanza.periodic
          | default
          = null,
        migrate
          | lib.contracts.Nullable stanza.migrate
          | default
          = null,
      },

    migrate = {
      health_check
        | [| 'checks, 'task_states |]
        | default
        = 'checks,
      healthy_deadline
        | std.number.Nat
        | default
        = 500000000000,
      max_parallel
        | std.number.Nat
        | default
        = 1,
      min_healthy_time
        | std.number.Nat
        | default
        = 10000000000,
    },

    periodic = {
      time_zone
        | String
        | default
        = "UTC",
      prohibit_overlap
        | Bool
        | default
        = false,
      cron | String,
    },

    affinity = {
      LTarget
        | String
        | default
        = null,
      RTarget | String,
      Operand
        | lib.contracts.OneOf
          [
            "regexp",
            "set_contains_all",
            "set_contains",
            "set_contains_any",
            "=",
            "==",
            "is",
            "!=",
            "not",
            ">",
            ">=",
            "<",
            "<=",
            "version"
          ]
        | default
        = "=",
      Weight
        | std.number.Nat
        | lib.contracts.NotEq 0
        | lib.contracts.GreaterEq
        - 100
        | lib.contracts.SmallerEq 100,
    },

    constraint = {
      attribute
        | String
        | default
        = null,
      value | String,
      operator
        | lib.contracts.OneOf
          [
            "=",
            "!=",
            ">",
            ">=",
            "<",
            "<=",
            "distinct_hosts",
            "distinct_property",
            "regexp",
            "set_contains",
            "version",
            "semver",
            "is_set",
            "is_not_set"
          ]
        | default
        = "=",
    },

    spread = {
      attribute
        | lib.contracts.Nullable String
        | default
        = null,
      weight
        | lib.contracts.Nullable
          (
            lib.contracts.AllOf
              [
                std.number.Nat,
                lib.contracts.GreaterEq - 100,
                lib.contracts.SmallerEq 100
              ]
          )
        | default
        = null,
      target
        | Array stanza.targetElem
        | default
        = [],
    },

    targetElem = {
      value
        | lib.contracts.Nullable String
        | default
        = null,
      percent
        | lib.contracts.Nullable
          (
            lib.contracts.AllOf
              [
                std.number.Nat,
                lib.contracts.GreaterEq - 100,
                lib.contracts.SmallerEq 100
              ]
          )
        | default
        = null,
    },

    ephemeral_disk =
      lib.contracts.Nullable
        {
          size | std.number.PosNat,
          migrate | Bool | default = false,
          sticky | Bool | default = false,
        },

    group = {
      type | [| 'service, 'batch, 'system |],
      affinities
        | Array stanza.affinity
        | default
        = [],
      constraints
        | Array stanza.constraint
        | default
        = [],
      spreads
        | Array stanza.spread
        | default
        = [],
      ephemeral_disk | stanza.ephemeral_disk | default = null,
      network
        | lib.contracts.Nullable stanza.network
        | default
        = null,
      service | { _ : stanza.service },
      task | { _ : stanza.task },
      count | std.number.Nat,
      volume | { _ : stanza.volume } | default = {},
      vault
        | lib.contracts.Nullable stanza.vault
        | default
        = null,
      restart
        | lib.contracts.Nullable stanza.restart
        | default
        = null,
      restart_policy
        | lib.contracts.Nullable stanza.restart
        | default
        = null,
      reschedule
        | stanza.reschedule
        & { type | [| 'service, 'batch, 'system |] }
        | default
        = {},
    },

    reschedule = {
      type
        | [| 'batch, 'service, 'system |]
        | default
        = 'service,

      # TODO: convert with ifs or sth
      # if #type == "batch" {
      #   attempts:       uint | *1
      #   delay:          #Duration | *"5s"
      #   delay_function: *"constant" | "exponential" | "fibonacci"
      #   interval:       #Duration | *"24h"
      #   unlimited:      bool | *false
      # }

      # if #type == "service" || type == "system" {
      #   interval:       #Duration | *"0m"
      #   attempts:       uint | *0
      #   delay:          #Duration | *"30s"
      #   delay_function: "constant" | *"exponential" | "fibonacci"
      #   max_delay:      #Duration | *"1h"
      #   // if unlimited is true, interval and attempts are ignored
      #   unlimited: bool | *true
      # }
    },

    network = {
      mode | [| 'host, 'bridge |],
      dns
        | lib.contracts.Nullable
          {
            servers
              | Array String
              | default
              = []
          }
        | default
        = null,
      port
        | {
          _ : {
            static
              | lib.contracts.Nullable std.number.Nat
              | default
              = null,
            to
              | lib.contracts.Nullable std.number.Nat
              | default
              = null,
            host_network
              | String
              | default
              = "",
          }
        },
    },

    check_restart = {
      limit | std.number.PosNat,
      grace | Duration,
      ignore_warnings
        | Bool
        | default
        = false,
    },

    service = {
      check_restart
        | lib.contracts.Nullable stanza.check_restart
        | default
        = null,
      port | String,
      address_mode
        | [| 'alloc, 'driver, 'auto, 'host |]
        | default
        = 'auto,
      tags
        | Array String
        | default
        = [],
      task
        | String
        | default
        = "",
      check | { _ : stanza.check } | default = {},
      meta | { _ : String } | default = {},
    },

    check = {
      address_mode
        | [| 'alloc, 'driver, 'host |]
        | default
        = 'driver,
      type | [| 'http, 'tcp, 'script, 'grpc |],
      port | String,
      interval | Duration,
      timeout | Duration,
      check_restart
        | lib.contracts.Nullable stanza.check_restart
        | default
        = null,
      header | { _ : Array String } | default = {},
      body
        | lib.contracts.Nullable String
        | default
        = null,
      initial_status
        | (lib.contracts.OneOf ["passing", "warning", "critical", ""])
        | default
        = "",
      success_before_passing
        | std.number.Nat
        | default
        = 0,
      failures_before_critical
        | std.number.Nat
        | default
        = 0,
      tls_skip_verify
        | Bool
        | default
        = false,

      #TODO: convert once we can do it
      #if type == "http" {
      #  method:   *"GET" | "POST"
      #  path:     string
      #  protocol: *"http" | "https"
      #}

      #if type != "http" {
      #  method:   ""
      #  path:     ""
      #  protocol: ""
      #}
      path | String | default = "",
      method | String | default = "",
      protocol | String | default = "",
    },

    #Hmm... actual union :( hard to do
    #taskConfig: dockerConfig | execConfig

    execConfig = {
      flake | String,
      command | String,
      args
        | Array String
        | default
        = [],
    },

    label = { _ : String },

    dockerConfig = {
      image | String,
      command
        | lib.contracts.Nullable String
        | default
        = null,
      args
        | Array String
        | default
        = [],
      ports
        | Array String
        | default
        = [],
      labels
        | Array label
        | default
        = [],
      logging | dockerConfigLogging
    },

    dockerConfigLogging = {
      type = "journald",
      config
        | Array dockerConfigLoggingConfig
        | default
        = [],
    },

    dockerConfigLoggingConfig = {
      tag | String,
      labels | String,
    },

    lifecycle = {
      hook | [| 'prestart, 'poststart, 'poststop |],
      sidecar
        | lib.contracts.Nullable Bool
        | default
        = null,
    },

    logs = {
      max_files | std.number.PosNat,
      max_file_size | std.number.PosNat,
    },

    task = {
      affinities
        | Array stanza.affinity
        | default
        = [],
      constraints
        | Array stanza.constraint
        | default
        = [],

      #TODO: contracts on field names
      #artifact: [Destination=_]: {
      artifact
        | {
          _ : {
            # actually, the key must be the destination
            # destination | Destination,
            headers | { _ : String },
            mode
              | [| 'any, 'file, 'dir |]
              | default
              = 'any,
            options | { _ : String },
            source | String,
          }
        }
        | default
        = {},

      #TODO: dependent if
      #if driver == "docker" {
      #  config: #stanza.dockerConfig
      #}

      #if driver == "exec" {
      #  config: #stanza.execConfig
      #}

      # meantime, allow config field:
      config | { .. },

      driver | [| 'exec, 'docker, 'nspawn |],

      env
        | { _ : String }
        | default
        = {},

      kill_signal
        | String
        | default
        = "SIGINT",

      #TODO
      #if driver == "docker" {
      #  kill_signal: "SIGTERM"
      #}

      kill_timeout
        | lib.contracts.Nullable Duration
        | default
        = null,

      lifecycle
        | lib.contracts.Nullable stanza.lifecycle
        | default
        = null,

      logs
        | lib.contracts.Nullable stanza.logs
        | default
        = null,

      resources = {
        cpu
          | std.number.Nat
          | lib.contracts.GreaterEq 100,
        memory
          | std.number.Nat
          | lib.contracts.GreaterEq 32,
      },

      #TODO: contracts on fields
      #template: [Destination=_]: {
      template
        | {
          _ : {
            # Actually, the key must be a destination, there is no
            # destination field
            # destination | Destination,
            data
              | String
              | default
              = "",
            source
              | String
              | default
              = "",
            env
              | Bool
              | default
              = false,
            change_mode
              | [| 'restart, 'noop, 'signal |]
              | default
              = 'restart,
            change_signal
              | String
              | default
              = "",
            perms
              | (lib.contracts.MatchRegexp "^[0-7]{4}$")
              | default
              = "0644",
            left_delimiter
              | String
              | default
              = "{{",
            right_delimiter
              | String
              | default
              = "}}",
            splay
              | Duration
              | default
              = "3s",
          }
        },

      vault
        | lib.contracts.Nullable stanza.vault
        | default
        = null,
      volume_mount | { _ : stanza.volume_mount } | default = {},
      restart
        | lib.contracts.Nullable stanza.restart
        | default
        = null,
      restart_policy
        | lib.contracts.Nullable stanza.restart
        | default
        = null,
      leader
        | Bool
        | default
        = false,

      ..
    },

    restart = {
      interval | Duration,
      attempts | std.number.PosNat,
      delay | Duration,
      mode | [| 'delay, 'fail |],
    },

    update = {
      auto_promote
        | Bool
        | default
        = false,
      auto_revert
        | Bool
        | default
        = false,
      canary
        | std.number.Nat
        | default
        = 0,
      health_check
        | [| 'checks, 'task_states, 'manual |]
        | default
        = 'checks,
      healthy_deadline
        | Duration
        | default
        = "5m",
      max_parallel
        | std.number.Nat
        | default
        = 1,
      min_healthy_time
        | Duration
        | default
        = "10s",
      progress_deadline
        | Duration
        | default
        = "10m",
      stagger
        | Duration
        | default
        = "30s",
    },

    vault = {
      change_mode
        | [| 'noop, 'restart, 'signal |]
        | default
        = 'restart,
      change_signal
        | String
        | default
        = "",
      env
        | Bool
        | default
        = true,
      namespace
        | String
        | default
        = "",
      policies
        | Array String
        | default
        = [],
    },

    volume = {
      type | [| 'host, 'csi |],
      source | String,
      read_only
        | Bool
        | default
        = false,
      #TODO: dependent if
      #if type == "csi" {
      #  mount_options: {
      #    fs_type:     *null | string
      #    mount_flags: *null | string
      #  }
      #}
    },

    volume_mount = {
      # Specifies the group volume that the mount is going to access.
      volume
        | String
        | default
        = "",

      # Specifies where the volume should be mounted inside the task's
      # allocation.
      destination
        | String
        | default
        = "",
      foo : String = destination ++ volume,

      # When a group volume is writeable, you may specify that it is read_only
      # on a per mount level using the read_only option here.
      read_only
        | Bool
        | default
        = false,
    },
  },
}
