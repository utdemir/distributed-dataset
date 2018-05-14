# serverless-execute

[![Build Status](https://travis-ci.org/utdemir/serverless-batch.svg?branch=master)](https://travis-ci.org/utdemir/serverless-batch) ![Hackage](https://img.shields.io/hackage/v/serverless-execute.svg)

Run arbitrary IO actions on Serverless offerings (currently only AWS Lambda).

Wikipedia [says](https://en.wikipedia.org/wiki/Serverless_computing):

> Serverless computing is a cloud-computing execution model in which the cloud provider dynamically manages the allocation of machine resources. Pricing is based on the actual amount of resources consumed by an application, rather than on pre-purchased units of capacity.

This library is an approach to leverage serverless computing platforms in a simple way in Haskell. The fundamental difference from the alternatives is that this library provides you the means of offloading some computation to AWS Lambda, rather than letting you deploy an application.

Simply, this library is a function vaguely similar to `execute :: IO a -> IO a`, which takes an IO action, runs the action remotely on a serverless offering and gives back the result.

It consist of a core API called 'serverless-execute' and various libraries implementing different `Backend`'s. Currently only supports AWS Lambda via 'serverless-execute-aws-lambda'.

## Examples

* [lambda.hs](https://github.com/utdemir/serverless-batch/blob/master/examples/lambda.hs): Tiny example for spawning a Lambda executor.
* [gh.hs](https://github.com/utdemir/serverless-batch/blob/master/examples/gh.hs): A more contrived example application that queries large amounts of data in parallel.

### Running Examples

* Make sure that you have AWS credentials set up. The easiest way is to installing AWS command line tools and running `aws configure`.

* Create a bucket on AWS to put the deployment artifact in. You can use the console or the command line tools:

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

* This is pretty new, so I am open to ideas, improvements or new backends.
