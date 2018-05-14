# serverless-execute

[![Build Status](https://travis-ci.org/utdemir/serverless-batch.svg?branch=master)](https://travis-ci.org/utdemir/serverless-batch)

Run arbitrary IO actions on Serverless offerings (currently only AWS Lambda).

Wikipedia [says](https://en.wikipedia.org/wiki/Serverless_computing):

> Serverless computing is a cloud-computing execution model in which the cloud provider dynamically manages the allocation of machine resources. Pricing is based on the actual amount of resources consumed by an application, rather than on pre-purchased units of capacity.[1] It is a form of utility computing.

This library is an approach to leverage serverless computing platforms in a simple way in Haskell. The fundamental difference between this library and the alternatives is that this library provides you the means of offloading some computation to AWS Lambda, rather than letting you deploy an application.

Simply, this library is a function vaguely similar to `execute :: IO a -> IO a`, which takes an IO action, runs the action on a serverless offering and gives back the result.

This library consist of a core API named 'serverless-execute' and various libraries implementing different `Backend`'s. Currenlty, we only support AWS Lambda via 'serverless-execute-aws-lambda'.

See:
* [https://github.com/utdemir/serverless-batch/blob/master/examples/lambda.hs](lambda.hs) for a small example
* [https://github.com/utdemir/serverless-batch/blob/master/examples/gh.hs](gh.hs) for a more contrived application.

# Roadmap

* My actual purpose is to create an alternative to [Apache Spark](https://spark.apache.org/) in Haskell. My plan for that is to use `serverless-batch` to replace executors.
