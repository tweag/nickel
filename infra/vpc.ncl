{
  naming_prefix | String,
  vpc = {
    id = "${resource.aws_vpc.runner_vpc.id}",
    subnet_id = "${resource.aws_subnet.runner_subnet.id}",
  },
  config = {
    resource.aws_vpc.runner_vpc = {
      cidr_block = "10.0.0.0/16",
      assign_generated_ipv6_cidr_block = true,
    },

    resource.aws_subnet.runner_subnet = {
      vpc_id = vpc.id,
      cidr_block = "${cidrsubnet(resource.aws_vpc.runner_vpc.cidr_block, 8, 0)}",
      ipv6_cidr_block = "${cidrsubnet(resource.aws_vpc.runner_vpc.ipv6_cidr_block, 8, 0)}",
      assign_ipv6_address_on_creation = true,
    },

    resource.aws_internet_gateway.runner_gw = {
      vpc_id = vpc.id,
    },

    resource.aws_egress_only_internet_gateway.runner_gw6 = {
      vpc_id = vpc.id,
    },

    resource.aws_route_table.runner_vpc_internet_route_table = {
      vpc_id = vpc.id,
    },

    resource.aws_route.runner_vpc_default_route = {
      route_table_id = "${resource.aws_route_table.runner_vpc_internet_route_table.id}",
      destination_cidr_block = "0.0.0.0/0",
      gateway_id = "${resource.aws_internet_gateway.runner_gw.id}",
    },

    resource.aws_route.runner_vpc_default6_route = {
      route_table_id = "${resource.aws_route_table.runner_vpc_internet_route_table.id}",
      destination_ipv6_cidr_block = "::/0",
      egress_only_gateway_id = "${resource.aws_egress_only_internet_gateway.runner_gw6.id}",
    },

    resource.aws_route_table_association.runner_vpc_internet_route_table_association = {
      subnet_id = vpc.subnet_id,
      route_table_id = "${resource.aws_route_table.runner_vpc_internet_route_table.id}"
    },
  }
}
