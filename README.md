haskell-kubernetes
==================

[![Build Status](https://secure.travis-ci.org/sferik/twitter.png?branch=master)][travis]
[![Code Climate](https://codeclimate.com/github/sferik/twitter.png)][codeclimate]
[![Coverage](https://coveralls.io/repos/sferik/twitter/badge.png?branch=master)][coveralls]

[travis]: http://travis-ci.org/sferik/twitter
[codeclimate]: https://codeclimate.com/github/sferik/twitter
[coveralls]: https://coveralls.io/r/sferik/twitter

This project contains haskell bindings to the `v1` Kubernetes api generated via [swagger-codegen](https://github.com/swagger-api/swagger-codegen). The client and (mock) server generated use the [servant](https://github.com/haskell-servant/servant) project, thanks to the work of [Masahiro Yamauchi](https://github.com/algas)!

### About the Generated Code

* `aeson` instances are generated for each type with the appropriate keys
* Lenses are generated for every field, with plain names for each lens (e.g. `min`, `object`, etc.). So be careful with unqualified imports.
* Fields specified as "not required" (missing a `"required"` field in the swagger description) are represented as a `Maybe`, even when that field refers to a list (e.g. `Maybe [Foo]`). This is done for consistent JSON parsing of the generated `aeson` instances.
* In lieu of suitable `Default` instances, builder methods are exported. Each datatype has a corresponding `mk{{datatype}}` method to construct it, requiring only required fields.
* API coverage is almost full. As `servant-0.4.4` lacks support for `HEAD` and `OPTIONS` endpoints, those endpoints were excluded from the **client** *and* **server** implementation (any related datatypes are still generated).
* client and server bindings are largely untested.

## Dependencies

_haskell-kubernetes_ depends on the following software:

* [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) `7.8.4` or `7.10.*`
* for code generation: [swagger-codegen](https://github.com/swagger-api/swagger-codegen).
* for code generation: [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) to clean up the generated code.
* for code generation: GNU `sed` (code generation may not work with the `sed` shipped with Mac OS X)

## Documentation

In addition to the `README` (this file), there is a [directory of additional documentation](/doc):

* The [Getting started](doc/GETTING_STARTED.md) information helps to understand limitations of the library, nuances about the generated datatypes, and the code generation process in general.
* The [Contributing to _project-name_](doc/CONTRIBUTING.md) page lists severals ways that you can improve _project-name_.
* The list of [potential improvements](doc/TODO.md) are prioritized.

## Versioning

_haskell-kubernetes_ adheres to Semantic Versioning 2.0.0. If there is a violation of this scheme, report it as a bug.Specifically, if a patch or minor version is
released and breaks backward compatibility, that version should be immediately yanked and/or a new version should be immediately released that restores
compatibility. Any change that breaks the public API will only be introduced at a major-version release. As a result of this policy, you can (and should)
specify any dependency on <haskell-kubernetes> by using the Pessimistic Version Constraint with two digits of precision.

## Licensing

See the [LICENSE](LICENSE.md) file for details.

<!-- The content of the LICENSE.md file is as follows: 
The MIT License (MIT)

Copyright &copy; 2014 _name-of-copyright-holder_

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. -->
 
