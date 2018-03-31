# serverless-*

An experiment to implement an Apache Spark like big data processing framework on top of Haskell. Some interesting features:

* Should run on various FaaS offerings (e.g. AWS Lambda, Google Cloud Functions etc.).
* Uses regular Haskell functions for data transformations, using `StaticPointers` and `distributed-closure`.

It consists of a few subprojects:

## serverless-execute

Run arbitrary IO actions on FaaS services, backend agnostic.

Status: Working prototype.

## serverless-execute-aws-lambda

An AWS Lambda backend for `serverless-execute`.

Status: Working prototype.

## serverless-batch

A DSL for describing data transformations.

Status: Design phase.