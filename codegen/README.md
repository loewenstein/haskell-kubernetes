Generating the Haskell Code
===========================

The client and server code generation mostly works. However, lots of massaging is needed on the Models. There is a bash script that one can run to handle this massaging (this should be done in `swagger-codegen` but would require some work on the haskell generation side).

## 1. Generate the code

We're going to generate the code and put the Model files in: `haskell-kubernetes/tmp123/`

1. `git clone https://github.com/swagger-api/swagger-codegen && cd swagger-codegen`
2. `mvn package`
3. `wget -o https://raw.githubusercontent.com/kubernetes/kubernetes/master/api/swagger-spec/v1.json`
3. `./run-in-docker.sh generate -t modules/swagger-codegen/src/main/resources/haskell-servant -i v1.json -l haskell-servant -DdebugModels -o ./dest/folder/`
4. `cp -R ./dest/folder/lib/Model/* ./tmp123/`

## 2. Move Models into repo

Here we run our bash tomfoolery to move the generated code into the write place and perform some semantic and cosmetic changes. Run from `haskell-kubernetes` root directory.

1. `./codegen/codegen.sh ./tmp123/`
