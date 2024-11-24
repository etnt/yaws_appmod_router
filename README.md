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
    yaws_appmod_router:add_route("GET", "/user/:id", fun simple:user/1, []),

    ...configure Yaws...

    ok.

%% Appmod handler function
root(_Arg) ->
    {content, "text/plain", "This is the '/' route !"}.

hello(_Arg) ->
    {content, "text/plain", "Hello this is the '/hello' route !"}.

user(Arg) ->
    Params = Arg#arg.appmoddata,
    UserId = maps:get(id, Params),
    {content, "text/plain", 
     io_lib:format("Hello user ~s! This is your profile page.", [UserId])}.
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
# Request 1
$ curl -is "http://127.0.0.1:8080/"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 00:05:29 GMT
Content-Length: 23
Content-Type: text/plain

This is the '/' route !


# Request 2
$ curl -is "http://127.0.0.1:8080/hello"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 00:05:36 GMT
Content-Length: 34
Content-Type: text/plain

Hello this is the '/hello' route !


# Request 3
$ curl -is "http://127.0.0.1:8080/user/bill"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 21:36:18 GMT
Content-Length: 43
Content-Type: text/plain

Hello user bill! This is your profile page.
```

## Example, using Middleware for authentication

Let us extend the simple example with a middleware that checks for valid
Basic Authentication credentials.

```erlang
start() ->
    ...code here as before...

    yaws_appmod_router:add_route("GET", "/login", fun simple:login/1, [
        fun simple:auth_middleware/1
    ]),

    ...code here as before...

%% Here comes some new code...
%%
%% Our authentication middleware checks for valid credentials
%% of a user with username "bill" and password "qwe123".
%% If the credentials are valid, the middleware adds a flag to the opaque
%% map of the request, indicating that the user is authenticated.
%% If the credentials are invalid, the middleware returns an error response.
%% which will cause the request to be terminated.
auth_middleware(Arg) ->
    Auth = yaws_api:headers_authorization(yaws_api:arg_headers(Arg)),
    case Auth of
        {"bill", "qwe123", _Orig} ->
            %% Add authenticated flag to opaque
            NewArg = update_opaque(Arg, authenticated, true),
            {ok, NewArg};
        Else ->
            unauthorized_response(Else)
    end.

unauthorized_response(_Else) ->
    Status = {status, 401},
    Headers = [{header, ["WWW-Authenticate: Basic realm=\"My Server\""]}],
    Html = "<html><body><h1>Authentication Failed</h1><p>Please provide valid credentials.</p></body></html>",
    {error, [Status | Headers] ++ [{content, "text/html", Html}]}.

update_opaque(#arg{opaque = OpaqueMap} = Arg, Key, Value) when is_map(OpaqueMap) ->
    Arg#arg{opaque = maps:put(Key, Value, OpaqueMap)}.

%% Protected login route
login(Arg) ->
    case maps:get(authenticated, Arg#arg.opaque, false) of
        true ->
            {content, "text/html",
                "<html><body><h1>Welcome!</h1><p>You have successfully authenticated.</p></body></html>"};
        _ ->
            {content, "text/html",
                "<html><body><h1>Error</h1><p>Authentication required.</p></body></html>"}
    end.
```

Start the server as before and then make some requests:

```bash
$ curl -is -u bill:asdf "http://127.0.0.1:8080/login"
HTTP/1.1 401 Unauthorized
Server: Yaws 2.2.0
Date: Sun, 24 Nov 2024 09:21:37 GMT
Content-Length: 96
Content-Type: text/html
WWW-Authenticate: Basic realm="My Server"

<html><body><h1>Authentication Failed</h1><p>Please provide valid credentials.</p></body></html>


$ curl -is -u bill:qwe123 "http://127.0.0.1:8080/login"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sun, 24 Nov 2024 09:21:46 GMT
Content-Length: 86
Content-Type: text/html

<html><body><h1>Welcome!</h1><p>You have successfully authenticated.</p></body></html>
```