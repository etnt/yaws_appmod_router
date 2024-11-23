-ifndef(_YAWS_APPMOD_ROUTER_).
-define(_YAWS_APPMOD_ROUTER_, true).

%% Records
-record(route, {
    method,
    path_pattern,
    handler,
    middlewares = [],
    params = #{}
}).

-endif.
