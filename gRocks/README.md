gRocks
=====

An OTP application that provides a web API with a single endpoint /process for computing the average of a list of numbers.

Build and Run
-----

    $ rebar3 deps
    $ rebar3 compile
    $ rebar3 eunit  # Run tests
    $ rebar3 shell  # Start the application

The server will listen on http://localhost:8080

API Usage
---------

POST /process

Content-Type: application/json

Body: [1,2,3]

Response: {"average": 2.0}

For invalid input, returns {"error": "reason"} with 400 status.
