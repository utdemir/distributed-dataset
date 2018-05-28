# distributed-fork

[![Build Status](https://travis-ci.org/utdemir/distributed-fork.svg?branch=master)](https://travis-ci.org/utdemir/distributed-fork) ![Hackage](https://img.shields.io/hackage/v/distributed-fork.svg)

Run arbitrary IO actions on remote machines.

This library leverages [StaticPointers language extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#static-pointers) and [distributed-closure library](https://hackage.haskell.org/package/distributed-closure) to run IO actions remotely.

It consist of a core API called 'disributed-fork' and various libraries implementing different `Backend`'s. Currently only supported `Backend` is 'distributed-fork-aws-lambda'.

Using 'distributed-fork-aws-lambda', you can use AWS's serverless computing offering called Lambda. This gives you the ability to run your functions in a scalable fashion without provisioning any server or infrastructure. This is especially useful if your function is [embarrassingly parallel](https://en.wikipedia.org/wiki/Embarrassingly_parallel):

  * Download lots of files and process them in parallel (See "Examples" section).
  * Load test an application by sending thousands of HTTP requests.
  * Run different iterations of a simulation in parallel.
  * Generate thumbnails for a set of images.

## Examples

* [lambda.hs](https://github.com/utdemir/serverless-batch/blob/master/examples/lambda.hs): Tiny example for spawning a Lambda executor.
* [gh.hs](https://github.com/utdemir/serverless-batch/blob/master/examples/gh.hs): A more contrived example application that queries large amounts of data in parallel.

### Running Examples

* Make sure that you have AWS credentials set up. The easiest way is to install [AWS command line interface](https://aws.amazon.com/cli/) and to run:

```
aws configure
```

* Create a bucket on AWS to put the deployment artifact in. You can use the console or the CLI:

```
aws s3api create-bucket --bucket my-s3-bucket
```

* Clone the repository

```
$ git clone https://github.com/utdemir/serverless-batch
$ cd serverless-batch
```

* Replace `my-s3-bucket` with the name of the bucket you just created:

```
vi examples/lambda.hs
```

* Build & run the example

```
$ stack build examples
$ stack exec lambda
```

## Contributing

I am open to contributions; any issue, PR or opinion is more than welcome.