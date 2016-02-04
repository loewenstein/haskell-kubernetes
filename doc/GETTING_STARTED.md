Getting Started
===============

## Use in a Haskell Project

For use in a Haskell project, simply include `haskell-kubernetes` in your `.cabal` or `stack.yaml`.

## Generate your own Kubernetes bindings!

We used [swagger-codegen](https://github.com/swagger-api/swagger-codegen) to generate the datatypes. We do some additional massaging and processing to deal with some of Kubernetes' nuances (they *really* like to prefix object names with `v1.`) and use a custom template to generate the Models.

To generate the source yourself:

### 1. Generate the code

We're going to generate the code and put the Model files in: `haskell-kubernetes/tmp123/`

1. `git clone https://github.com/swagger-api/swagger-codegen && cd swagger-codegen`
2. `mvn package`
3. `wget -o https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/swagger-spec/v1.json`
3. `./run-in-docker.sh generate -t modules/swagger-codegen/src/main/resources/haskell-servant -i v1.json -l haskell-servant -DdebugModels -o ./dest/folder/`
4. `cp -R ./dest/folder/lib/Model/* ./tmp123/`

## 2. Move Models into repo

Here we run our bash tomfoolery to move the generated code into the write place and perform some semantic and cosmetic changes. Run from `haskell-kubernetes` root directory.

1. `./codegen/codegen.sh ./tmp123/`
