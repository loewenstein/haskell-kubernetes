Haskell-Kubernetes
==================

This project contains haskell bindings to the `v1` Kubernetes api generated via [swagger-codegen](https://github.com/swagger-api/swagger-codegen). The client and (mock) server generated use the [servant](https://github.com/haskell-servant/servant) project, thanks to the work of [Masahiro Yamauchi](https://github.com/algas)!

## About the Datatypes Generated

- Non-required fields are represented as a `Maybe`, even when that field contains a list (e.g. `Maybe [Foo]`).
- `aeson` instances are generated for each type with the appropriate keys
- Lenses are generated for every field, with plain names for each lens (e.g. `min`, `object`, etc.). So be careful with unqualified imports.

## About the Code Generation Process

We used [swagger-codegen](https://github.com/swagger-api/swagger-codegen) to generate the datatypes. We do some additional massaging and processing to deal with some of Kubernetes' nuances (they *really* like to prefix object names with `v1.`) and use a custom template to generate the Models.

Most additional processing is documented in the `codegen` folder.

## Limitations of this Project

1. client and server bindings are largely untested.
2. API coverage is almost full. As `servant-0.4.4` lacks support for `HEAD` and `OPTIONS` endpoints, those endpoints were excluded from the **client** *and* **server** implementation (though any related datatypes are still generated).
3. The `server` executable takes over 15 minutes to compile.
