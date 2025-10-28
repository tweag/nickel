# test = 'pass'
let
  kelvin_to_celsius = fun kelvin => kelvin - 273.15,
  kelvin_to_fahrenheit = fun kelvin => (kelvin - 273.15) * 1.8000 + 32.00,
in

let
  melting_celsius = kelvin_to_celsius 1728,
  melting_fahrenheit = kelvin_to_fahrenheit 1728,
  boiling_celsius = kelvin_to_celsius 3003,
  boiling_fahrenheit = kelvin_to_fahrenheit 3003,
in

{
  physical = {
    phase_at_STP = "solid",
    melting_point = "1728 K (%{melting_celsius} °C, %{melting_fahrenheit} °F)",
    boiling_point = "3003 K (%{boiling_celsius} °C, %{boiling_fahrenheit} °F)",
    density = "8.908 g/cm3",
    when_liquid = "7.81 g/cm3",
    heat_of_fusion = "17.48 kJ/mol",
    heat_of_vaporization = "379 kJ/mol",
    molar_heat_capacity = "26.07 J/(mol·K)"
  }
}
