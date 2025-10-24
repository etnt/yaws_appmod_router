# yaws_appmod_router
> A simple Yaws appmod that routes requests.


Implements a routing capability for the Erlang Yaws web server as an appmod.

The router allow:

* Defining routes with path patterns.
* Associating middleware and callbacks with those routes.
* Specifying HTTP methods (GET, POST, etc.)
* CRUD routes: shortcut for defining routes for Create, Read, Update and Delete.
* Multiple routing tables for different IP/Port combinations.

Example usage:

```erlang
my_router:init().
my_router:add_route("GET", "/hello", fun my_handler:hello/1).
my_router:add_route("POST", "/submit", fun my_handler:submit/1, [auth_middleware]).
```

## Install and use

After cloning run:

1. **make**
2. **make test**

Study the examples for how to setup Yaws and the yaws_appmod_router.

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
$ make starti
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
$ curl -is  "http://127.0.0.1:8080/user/bill"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sat, 23 Nov 2024 21:36:18 GMT
Content-Length: 43
Content-Type: text/plain

Hello user bill! This is your profile page.
```

## Example, using Middleware for authentication

Let us modify the simple example with a middleware that checks for valid
Basic Authentication credentials.

```erlang
start() ->
    ...code here as before...

    yaws_appmod_router:add_route("GET", "/user/:id", fun simple:user/1, [
        fun simple:authenticate/1
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
authenticate(Arg) ->
    Auth = yaws_api:headers_authorization(yaws_api:arg_headers(Arg)),
    case Auth of
        {"bill", "qwe123", _Orig} ->
            %% Add authenticated flag to opaque
            NewArg = update_opaque(Arg, authenticated, true),
            {ok, NewArg};
        Else ->
            unauthorized_response(Else)
    end.

%% Here is our route handler function demonstrating parameter extraction.
%% NOTE: We also check that the user has been authenticated by the Middleware.
user(#arg{opaque     = OpaqueMap,
          appmoddata = Params} = _Arg) ->
    case maps:get(authenticated, OpaqueMap, false) of
        false ->
            %% NOTE: We won't really end up here since our Middleware will
            %% return an error response if the user is not authenticated.
            %% But in case it would mal-function we return a generic error.
            [{status, 401},
             {content, "text/html", "<h1>Error</h1><p>Authentication required.</p>"}];
        true ->
            UserId = maps:get(id, Params),
            {content, "text/plain",
                io_lib:format("Hello user ~s! This is your profile page.", [UserId])}
    end.

```

Start the server as before and then make some requests:

```bash
# Request 4
$ curl -is -u bill:asdf "http://127.0.0.1:8080/user/bill"
HTTP/1.1 401 Unauthorized
Server: Yaws 2.2.0
Date: Sun, 24 Nov 2024 09:21:37 GMT
Content-Length: 96
Content-Type: text/html
WWW-Authenticate: Basic realm="My Server"

<html><body><h1>Authentication Failed</h1><p>Please provide valid credentials.</p></body></html>


# Request 5
$ curl -is -u bill:qwe123 "http://127.0.0.1:8080/user/bill"
HTTP/1.1 200 OK
Server: Yaws 2.2.0
Date: Sun, 24 Nov 2024 09:21:46 GMT
Content-Length: 86
Content-Type: text/html

Hello user bill! This is your profile page.
```

## The Yaws Mod:auth/2 and Mod:out/1 appmod callbacks

Yaws provides two appmod callback functions that can be invoked.
The `auth/2` callback is invoked before the out/1 callback and is
used to check if the request is authorized. The `out/1` callback
is invoked after the auth/2 callback and is used to handle the
request.

With the yaws_appmod_router, we don't need the `auth/2` callback
since we can use our own authentication middleware to check if the
user is authenticated. However if you already have an existing system
where you need to use the `auth/2` callback, you can still use the
yaws_appmod_router to handle the authentication.

By enabling the `yaws_appmod_app_auth` flag in the opaque map of the
request, the yaws_appmod_router will handle the `auth/2` callback
and invoke your route handler function. Again see the 
[simple example](./examples/simple.erl), for the details.
Below, we highlight the important bits:

```erlang
    ....
    %% NOTE: The login handler will be called both via the auth/2 callback
    %% and via the out/1 callback.
    yaws_appmod_router:add_route("GET", "/login", fun simple:login/1, []),

    ...setting up Yaws...
    Docroot = "/tmp",
    Realm = "Simple",
    Type = "Basic",
    %% NOTE: We enable auth/2 callback handling from root here.
    Auth = #auth{dir="/",
                docroot=Docroot,
                realm=Realm,
                type=Type,
                headers = ["WWW-Authenticate: "++Type++" realm=\""++Realm++"\"\r\n"],
                mod=yaws_appmod_router},
    OpaqueMap = #{},
    SconfList = [
        {port, 8080},
        {servername, "simple_server"},
        {listen, {127, 0, 0, 1}},
        {docroot, Docroot},
        %% NOTE: We enable auth/2 callback handling here.
        {opaque, yaws_appmod_router:enable_auth(OpaqueMap)},
        {auth, Auth},
        %% NOTE: We hook in the yaws_appmod_router appmod here at root.
        {appmods, [{"/", yaws_appmod_router}]}
    ],
    ....


%% Login route handler, protected by auth/2 callback
login(#arg{} = _Arg) ->
    %% NOTE: this is executed during the out/1 callback.
    {content, "text/html",
        "<html><body><h1>Welcome!</h1><p>You have successfully authenticated.</p></body></html>"};
%%
login({Arg, _Auth}) ->
    %% NOTE: This is executed during the auth/2 callback, so we can only
    %% return values that are valid according to the Yaws auth/2 callback.
    case authenticate(Arg) of
        {ok, _} -> true;
        _       -> false
    end.
```

Note the two clauses of the login route handler function. One clause will
match when the auth/2 callback is invoked and the other when the out/1 callback
is invoked.

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
# Request 6
$ curl -is -X OPTIONS  http://localhost:8080/api/users
HTTP/1.1 204 No Content
Server: Yaws 2.2.0
Date: Tue, 26 Nov 2024 19:57:08 GMT
Allow: POST, GET, PUT, PATCH, DELETE, OPTIONS
```

A CORS preflight request:

```bash
# Request 7
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

## Multiple route tables

Yaws makes it possible to listen to multiple IP/Port combinations. Hence we can setup
completely separate routing tables for each IP/Port combination.

See the [multi routing](./examples/multi_routing.erl) example, for how to setup this.

We show the important bits here. We create two separate routing tables, one for
each IP/Port combination. We then add routes to each table.

It is important to set the `table_name` in the opaque map of the Sconf, else
the yaws_appmod_router will not be able to find the correct table to use.

```erlang
setup_servers() ->
    IP = {127,0,0,1},
    Port8080 = 8080,
    Port9999 = 9999,
    TableName8080 = yaws_appmod_router:mk_table_name(IP, Port8080),
    TableName9999 = yaws_appmod_router:mk_table_name(IP, Port9999),

    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(TableName8080),
    yaws_appmod_router:init(TableName9999),

    yaws_appmod_router:table_add_route(
                                 TableName8080,
                                 "CRUD", "/api/workers",
                                 %% Handler
                                 fun ...
                                 %% Middlewares
                                 [ ... ]),

    yaws_appmod_router:table_add_route(
                                    TableName9999,
                                    "CRUD", "/api/users",
                                    %% Handler
                                    fun ...
                                    %% Middlewares
                                    [ ... ]),
    ...

    setup_server(IP, {Port8080, TableName8080}, {Port9999, TableName9999}).

setup_server(IP, {Port1, TableName1}, {Port2, TableName2}) ->
    Id = "multi_routing",
    GconfList = [{id, Id}],
    Docroot = "/tmp",
    SconfList =
        [
            {port, Port1},
            {servername, "server1"},
            {listen, IP},
            {docroot, Docroot},
            %% NOTE: It is important to set the table name here in the opaque map,
            %% else the yaws_appmod_router will not be able to find what table to use.
            {opaque, #{table_name => TableName1}},
            {appmods, [{"/", yaws_appmod_router}]}
        ],

    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),

    ...continue to setup more servers...
```
