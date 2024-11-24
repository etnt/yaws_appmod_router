# yaws_appmod_router
> A simple Yaws appmod that routes requests.

## Sketch

To implement a routing capability for the Erlang Yaws web server, you can create a
custom appmod that provides an intuitive API for specifying routes and their
associated handlers. The design will include path pattern matching, middleware
 execution, and a mechanism to handle the responses.

The aim is a modular design that allows you to easily add new routes,
handlers, and middleware while keeping the Yaws appmod simple and reusable.

Here’s how you can design and implement such a routing appmod:

### Define the Router API

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

### CRUD routes

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

### Core Components

1. __Route Matching__: Use pattern matching to compare incoming requests against a list of registered routes.

2. __Middleware Support__: Allow optional middleware functions to process requests before reaching the route handler.

3. __Dynamic Path Segments__: Allow capturing dynamic segments like /user/:id.

### Implementation

Here’s a step-by-step implementation in Erlang.

1. Router State

The router will maintain a state with registered routes. Each route can have:

* HTTP method.
* Path pattern.
* Handler function.
* Middleware functions.

Define the state as:

```erlang
-record(route, {
    method :: atom(),
    path_pattern :: string(),
    handler :: fun(),
    middlewares :: [fun()]
}).

%% The state will hold a list of routes
-define(STATE, []).

-module(my_router).
-export([init/0, add_route/4, appmod/2]).
```

2. Router Initialization

Set up a state for storing routes.

```erlang
init() ->
    erlang:put(routes, ?STATE).
```

3. Add Route Function

Add routes to the state.

```erlang
add_route(Method, PathPattern, Handler, Middlewares) ->
    Route = #route{
        method = Method,
        path_pattern = PathPattern,
        handler = Handler,
        middlewares = Middlewares
    },
    Routes = erlang:get(routes),
    erlang:put(routes, [Route | Routes]).
```

4. Route Matching

Match an incoming request to the appropriate route using path pattern matching.

 ```erlang
 find_route(Method, Path) ->
    Routes = erlang:get(routes),
    lists:filter(
      fun(#route{method = Method, path_pattern = PathPattern} = Route) ->
          match_path(PathPattern, Path)
      end,
      Routes).

match_path(PathPattern, Path) ->
    %% Example: match "/user/:id" with "/user/123"
    SegmentsPattern = string:split(PathPattern, "/"),
    SegmentsPath = string:split(Path, "/"),
    length(SegmentsPattern) == length(SegmentsPath) andalso
    lists:all(fun match_segment/2, lists:zip(SegmentsPattern, SegmentsPath)).

match_segment(":" ++ _, _) -> true;
match_segment(Seg, Seg) -> true;
match_segment(_, _) -> false.
```

5. Middleware Execution

Execute middleware functions in sequence before invoking the handler.

 ```erlang
 execute_middlewares([], Req) -> {ok, Req};
execute_middlewares([MW | Rest], Req) ->
    case MW(Req) of
        {ok, Req1} -> execute_middlewares(Rest, Req1);
        {error, Reason} -> {error, Reason}
    end.
```

6. Appmod Callback

Define the appmod callback to handle requests.

```erlang
appmod(Req, State) ->
    case find_route(Req#arg.method, Req#arg.path) of
        [#route{handler = Handler, middlewares = Middlewares}] ->
            case execute_middlewares(Middlewares, Req) of
                {ok, UpdatedReq} ->
                    Handler(UpdatedReq);
                {error, Reason} ->
                    {html, io_lib:format("<h1>Error: ~p</h1>", [Reason])}
            end;
        [] ->
            {html, "<h1>404 Not Found</h1>"}
    end.
```

### Example Handler

Handlers process requests and return responses.

```erlang
-module(my_handler).
-export([hello/1, submit/1]).

hello(Req) ->
    {ok, Name} = yaws_api:parse_query_param(Req, "name"),
    {html, io_lib:format("Hello, ~s!", [Name])}.

submit(Req) ->
    {ok, Body} = yaws_api:read_body(Req),
    {html, io_lib:format("Received: ~p", [Body])}.
```

### Example Middleware

Middlewares preprocess requests.

```erlang
-module(auth_middleware).
-export([auth/1]).

auth(Req) ->
    case yaws_api:get_header("authorization", Req) of
        undefined ->
            {error, "Unauthorized"};
        Auth ->
            {ok, Req#arg{headers = [{auth, Auth} | Req#arg.headers]}}
    end.
```
