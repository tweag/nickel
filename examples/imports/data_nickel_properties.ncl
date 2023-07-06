# test = 'pass'
let kelvin_to_celcius = fun kelvin => kelvin - 273.15 in
let kelvin_to_fahrenheit = fun kelvin => (kelvin - 273.15) * 1.8000 + 32.00 in

let melting_celcius = std.string.from_number (kelvin_to_celcius 1728) in
let melting_fahrenheit = std.string.from_number (kelvin_to_fahrenheit 1728) in

let boiling_celcius = std.string.from_number (kelvin_to_celcius 3003) in
let boiling_fahrenheit = std.string.from_number (kelvin_to_fahrenheit 3003) in

{
  physical = {
    phase_at_STP = "solid",
    melting_point = "1728 K (%{melting_celcius} °C, %{melting_fahrenheit} °F)",
    boiling_point = "3003 K (%{boiling_celcius} °C, %{boiling_fahrenheit} °F)",
    density = "8.908 g/cm3",
    when_liquid = "7.81 g/cm3",
    heat_of_fusion = "17.48 kJ/mol",
    heat_of_vaporization = "379 kJ/mol",
    molar_heat_capacity = "26.07 J/(mol·K)"
  }
}
