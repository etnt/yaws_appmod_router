-ifndef(_YAWS_APPMOD_ROUTER_).
-define(_YAWS_APPMOD_ROUTER_, true).

%% Records
-record(route, {
    method,
    crud             :: undefined | yaws_appmod_router:crud(),
    action           :: yaws_appmod_router:actions(),
    path_pattern,
    handler,
    middlewares = [],
    params = #{}
}).

-endif.
