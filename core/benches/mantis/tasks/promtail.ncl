let types = import "../schemas/nomad/types.ncl" in
types.stanza.task
& {
  driver = 'exec,

  resources = {
    cpu = 100,
    memory = 61,
  },

  config = {
    flake = "github:input-output-hk/mantis-ops/cue#grafana-loki",
    command = "/bin/promtail",
    args = ["-config.file", "local/config.yaml"],
  },

  template."local/config.yaml".data =
    std.serialize
      'Yaml
      {
        server = {
          http_listen_port = 0,
          grpc_listen_port = 0,
        },
        positions.filename = "/local/positions.yaml",
        client.url = "http://172.16.0.20:3100/loki/api/v1/push",
        scrape_configs = [
          {
            job_name = "{{ env \"NOMAD_JOB_NAME\" }}-{{ env \"NOMAD_ALLOC_INDEX\"
}}",
            pipeline_stages = null,
            static_configs = [
              {
                labels = {
                  nomad_alloc_id = "{{ env \"NOMAD_ALLOC_ID\" }}",
                  nomad_alloc_index = "{{ env \"NOMAD_ALLOC_INDEX\" }}",
                  nomad_alloc_name = "{{ env \"NOMAD_ALLOC_NAME\" }}",
                  nomad_dc = "{{ env \"NOMAD_DC\" }}",
                  nomad_group_name = "{{ env \"NOMAD_GROUP_NAME\" }}",
                  nomad_job_id = "{{ env \"NOMAD_JOB_ID\" }}",
                  nomad_job_name = "{{ env \"NOMAD_JOB_NAME\" }}",
                  nomad_job_parent_id = "{{ env \"NOMAD_JOB_PARENT_ID\" }}",
                  nomad_namespace = "{{ env \"NOMAD_NAMESPACE\" }}",
                  nomad_region = "{{ env \"NOMAD_REGION\" }}",
                  # This is currently always "promtail", so this label wouldn't be of any use
                  # nomad_task_name:     "{{ env \"NOMAD_TASK_NAME\" }}"
                  "__path__" = "/alloc/logs/*.std*.[0-9]*",
                }
              }
            ]
          }
        ]
      }
}
