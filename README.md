# distributed-dataset

[![Documentation](https://img.shields.io/badge/-api%20documentation-informational.svg)](https://utdemir.github.io/distributed-dataset/)
[![Build Status](https://travis-ci.org/utdemir/distributed-dataset.svg?branch=master)](https://travis-ci.org/utdemir/distributed-dataset)

A distributed data processing framework in pure Haskell. Inspired by [Apache Spark](https://spark.apache.org/).

[An example is worth a thousand words](/examples/gh/Main.hs).

## Packages

### distributed-dataset

This package provides a `Dataset` type which lets you express and execute
transformations on a distributed multiset. Its API is highly inspired
by Apache Spark.

It uses pluggable `Backend`s for spawning executors and `ShuffleStore`s
for exchanging information. See 'distributed-dataset-aws' for an
implementation using AWS Lambda and S3.

It also exposes a more primitive `Control.Distributed.Fork`
module which lets you run `IO` actions remotely. It
is especially useful when your task is [embarrassingly
parallel](https://en.wikipedia.org/wiki/Embarrassingly_parallel).

### distributed-dataset-aws

This package provides a backend for 'distributed-dataset' using AWS
services. Currently it supports running functions on AWS Lambda and
using an S3 bucket as a shuffle store.

### distributed-dataset-opendatasets

Provides `Dataset`'s reading from public open datasets. Currently it can fetch GitHub event data from [GH Archive](https://www.gharchive.org).

## Running the example

* Clone the repository.

  ```sh
  $ git clone https://github.com/utdemir/distributed-dataset
  $ cd distributed-dataset
  ```

* Make sure that you have AWS credentials set up. The easiest way is
  to install [AWS command line interface](https://aws.amazon.com/cli/)
  and to run:

  ```sh
  $ aws configure
  ```

* Create an S3 bucket to put the deployment artifact in. You can use
  the console or the CLI:

  ```sh
  $ aws s3api create-bucket --bucket my-s3-bucket
  ```

* Build an run the example:

  * If you use Nix on Linux: 
     
    * (Optional) Use my binary cache on Cachix to reduce compilation times:

    ```sh
    $(nix-build -A cachix https://cachix.org/api/v1/install)/bin/cachix use utdemir
    ```
 
    * Then:
    
      ```sh
      $ $(nix-build -A example-gh)/bin/example-gh my-s3-bucket
      ```
    
  * If you use stack (requires Docker, works on Linux and MacOS): 

      ```sh
      $ stack run --docker-mount $HOME/.aws/ --docker-env HOME=$HOME example-gh my-s3-bucket
      ```

## Stability

Experimental. Expect lots of missing features, bugs,
instability and API changes. You will probably need to
modify the source if you want to do anything serious. See
[issues](https://github.com/utdemir/distributed-dataset/issues).

## Contributing

I am open to contributions; any issue, PR or opinion is more than welcome.

* In order to develop `distributed-dataset`, you can use;
  * On Linux: `Nix`, `cabal-install` or `stack`.
  * On MacOS: `stack` with `docker`.
* Use [ormolu](https://github.com/tweag/ormolu) to format source code.

### Nix

* You can use [my binary cache on cachix](https://utdemir.cachix.org/)
  so that you don't recompile half of the Hackage.
* `nix-shell` will drop you into a shell with `ormolu`, `cabal-install`,
  `.ghcid` alongside with all required haskell and system dependencies. 
  You can use `cabal new-*` commands there.
* There is a `./make.sh` at the root folder with some utilities like
  formatting the source code or running `ghcid`, run `./make.sh --help`
  to see the usage.

### Stack

* Make sure that you have `Docker` installed.
* Use `stack` as usual, it will automatically use a Docker image
* Run `./make.sh stack-build` before you send a PR to test different resolvers.

## Related Work

### Papers

* [Towards Haskell in Cloud](https://www.microsoft.com/en-us/research/publication/towards-haskell-cloud/) by Jeff Epstein, Andrew P. Black, Simon L. Peyton Jones 
* [Resilient Distributed Datasets: A Fault-Tolerant Abstraction for In-Memory Cluster Computing](https://cs.stanford.edu/~matei/papers/2012/nsdi_spark.pdf) by Matei Zaharia, et al.

### Projects

* [Apache Spark](https://spark.apache.org/).
* [Sparkle](https://github.com/tweag/sparkle): Run Haskell on top of Apache Spark.
* [HSpark](https://github.com/yogeshsajanikar/hspark): Another attempt at porting Apache Spark to Haskell.

