# yaws_appmod_router
> A simple Yaws appmod that routes requests.


Implements a routing capability for the Erlang Yaws web server as an appmod.

The router should allow:

* Defining routes with path patterns.
* Associating middleware and callbacks with those routes.
* Specifying HTTP methods (GET, POST, etc.)

Example usage:

```erlang
my_router:init().
my_router:add_route("GET", "/hello", fun my_handler:hello/1).
my_router:add_route("POST", "/submit", fun my_handler:submit/1, [auth_middleware]).
```

## Simple Example

First compile everything by running: `make old`

Have a look at the `examples/simple.erl`, the important bits are:

```erlang
-module(simple).
-export([start/0]).
-export([root/1
        , hello/1]).

%% Start Yaws in embedded mode with appmod configuration
start() ->
    ...setup Yaws...

    yaws_appmod_router:init(),
    yaws_appmod_router:add_route("GET", "/", fun simple:root/1, []),
    yaws_appmod_router:add_route("GET", "/hello", fun simple:hello/1, []),

    ...configure Yaws...

    ok.

%% Appmod handler function
root(_Arg) ->
    {content, "text/plain", "This is the '/' route !"}.

hello(_Arg) ->
    {content, "text/plain", "Hello this is the '/hello' route !"}.
```

Now start our server:

```bash
# Start the simple example
$ make shell
1> simple:start().
Yaws server started on port 8080 
```

Make some requests, using Curl:

```bash
$ curl -is "http://127.0.0.1:8080/"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 00:05:29 GMT
Content-Length: 23
Content-Type: text/plain

This is the '/' route !


$ curl -is "http://127.0.0.1:8080/hello"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 00:05:36 GMT
Content-Length: 34
Content-Type: text/plain

Hello this is the '/hello' route !
```
