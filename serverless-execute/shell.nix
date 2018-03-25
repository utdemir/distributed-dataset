{ compiler ? "ghc822" }:

(import ../. { inherit compiler; }).serverless-execute.env
