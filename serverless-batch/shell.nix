{ compiler ? "ghc822" }:

(import ../. { inherit compiler; }).serverless-batch.env
