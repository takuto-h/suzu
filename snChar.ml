
let is_digit c = String.contains "0123456789" c

let int_of_digit c = Char.code c - Char.code '0'
