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

Have a look at the [simple example](./examples/simple.erl), the important bits are:

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

## More examples

For more examples have a look at the [advanced routing](./examples/advanced_routing.erl) example.

## CRUD routes

An alternative to specify each HTTP method we can make use of
the `CRUD` specification. This way we can specify just a single route
and the **yaws_appmod_router** will automatically indicate the appropriate
action by setting the key `action` in the `appmoddata` map.

The `CRUD` string is interpreted as the four operations:
Create, Read, Update and Delete. Each letter can be omitted
to indicate what actions is not supported for that path.

The following table shows the supported CRUD actions.

| **CRUD**  | **Path**        | **Method**   | **Action** |
|-----------|-----------------|--------------|------------|
| (R)ead    | `/users`        | GET          | index      |
|           | `/users/:id`    | GET          | show       |
| (C)reate  | `/users`        | POST         | create     |
| (U)pdate  | `/users/:id`    | PUT          | replace    |
|           | `/users/:id`    | PATCH        | modify     |
| (D)elete  | `/users/:id`    | DELETE       | delete     |

See also the [CRUD routing](./examples/crud_routing.erl) example.

```erlang
%% Example 1: Full CRUD specification
add_route("CRUD", "/api/users", fun mymod:handle_users/1, [Middlewares...]).

handle_users(#arg{appmoddata = Map} = Arg) ->
    case maps:get(action, Map) of
        index   -> get_users(Arg);
        show    -> get_user(Arg);
        create  -> create_user(Arg);
        replace -> replace_user(Arg);
        modify  -> modify_user(Arg);
        delete  -> delete_user(Arg);
        _       -> method_not_allowed(Arg)
    end.


%% Example 2: Only a CD specification
add_route("CD", "/api/person", fun mymod:handle_person/1, [Middlewares...]).

handle_person(#arg{appmoddata = Map} = Arg) ->
    case maps:get(action, Map) of
        create -> create_person(Arg);
        delete -> delete_person(Arg);
        _      -> method_not_allowed(Arg)
    end.
```

When using a CRUD route specification, **yaws_appmod_router** will automatically
answer to `OPTIONS` requests and return the allowed methods for the given route.

```bash
$ curl -is -X OPTIONS  http://localhost:8080/api/users
HTTP/1.1 204 No Content
Server: Yaws 2.2.0
Date: Tue, 26 Nov 2024 19:57:08 GMT
Allow: POST, GET, PUT, PATCH, DELETE, OPTIONS
```

A CORS preflight request:

```bash
$ curl -is -X OPTIONS -H 'Access-Control-Request-Method: POST'  http://localhost:8080/api/users
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Tue, 26 Nov 2024 22:41:12 GMT
Content-Type: text/html 
Access-Control-Allow-Origin: *
Access-Control-Allow-Methods: GET, POST, PUT, PATCH, DELETE, OPTIONS
```

From an Erlang shell we can print the route table:

```erlang
1> yaws_appmod_router:print_routes().
METHOD   PATH PATTERN                             CRUD  ACTION    HANDLER
------   ------------                             ----  ------    -------
GET      /api/semaphores                          CRD   index     fun crud_routing:handle_semaphores/1
OPTIONS  /api/semaphores                          CRD   options   undefined
POST     /api/semaphores                          CRD   create    fun crud_routing:handle_semaphores/1
DELETE   /api/semaphores/:id                      CRD   delete    fun crud_routing:handle_semaphores/1
GET      /api/semaphores/:id                      CRD   show      fun crud_routing:handle_semaphores/1
GET      /api/workers                             CRUD  index     fun crud_routing:handle_workers/1
OPTIONS  /api/workers                             CRUD  options   undefined
POST     /api/workers                             CRUD  create    fun crud_routing:handle_workers/1
DELETE   /api/workers/:id                         CRUD  delete    fun crud_routing:handle_workers/1
GET      /api/workers/:id                         CRUD  show      fun crud_routing:handle_workers/1
PATCH    /api/workers/:id                         CRUD  modify    fun crud_routing:handle_workers/1
PUT      /api/workers/:id                         CRUD  replace   fun crud_routing:handle_workers/1
ok
```