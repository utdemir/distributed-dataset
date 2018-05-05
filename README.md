# serverless-*

An experiment to implement an Apache Spark like big data processing framework on top of Haskell. Goals:

* Should run on various FaaS offerings (e.g. AWS Lambda, Google Cloud Functions etc.).
* Uses regular Haskell functions for data transformations, using `StaticPointers` and `distributed-closure`.

It consists of a few subprojects:

## serverless-execute

Run arbitrary IO actions on FaaS services, backend agnostic.

### serverless-execute-aws-lambda

An AWS Lambda backend for `serverless-execute`.

## serverless-batch

A DSL for describing data transformations.

Status: Design phase.

# TODO

* Working prototype of serverless-batch.
* Make Stack work. The main problem is getting the dependencies compiling.
* Make sure everything compiles on Travis. Currently stack does not work and Nix times out.
* Create another backend for `serverless-execute` to see if the abstraction holds.
* Document everything.
* Try to get free credits from a cloud provider.
* Performance benchmarks.
