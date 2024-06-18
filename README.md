# repeater

This crate provides the `repeat!` macro, which lets you repeat tokens. In its simplest form,
`repeat!` takes a repeat count and the tokens to repeat:

`repeat!(5 => #(repeat this 5 times)*)`

See the documentation for more advanced usage, including loop variables, interpolations
and nesting.

## Future plans

- Non-integer loop variables, array syntax
