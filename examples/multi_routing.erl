-module(multi_routing).
-export([start/0]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").


start() ->
    setup_yaws(),
    setup_servers().


setup_servers() ->
    IP = {127,0,0,1},
    Port8080 = 8080,
    Port9999 = 9999,
    TableName8080 = yaws_appmod_router:mk_table_name(IP, Port8080),
    TableName9999 = yaws_appmod_router:mk_table_name(IP, Port9999),

    {ok, _} = application:ensure_all_started([yaws, yaws_appmod_router]),
    yaws_appmod_router:init(TableName8080),
    yaws_appmod_router:init(TableName9999),

    %% NOTE: We are reusing the handlers and middlewares from the crud example.
    yaws_appmod_router:table_add_route(
                                 TableName8080,
                                 "CRUD", "/api/workers",
                                 %% Handler
                                 fun crud_routing:handle_workers/1,
                                 %% Middlewares
                                 [
                                  fun crud_routing:logger_middleware/1,
                                  fun crud_routing:cors_middleware/1,
                                  fun crud_routing:json_post_data_middleware/1
                                 ]),

    yaws_appmod_router:table_add_route(
                                 TableName8080,
                                 "CRD", "/api/semaphores",
                                 %% Handler
                                 fun crud_routing:handle_semaphores/1,
                                 %% Middlewares
                                 [
                                  fun crud_routing:logger_middleware/1,
                                  fun crud_routing:cors_middleware/1
                                 ]),

    yaws_appmod_router:table_add_route(
                                    TableName9999,
                                    "CRUD", "/api/users",
                                    %% Handler
                                    fun crud_routing:handle_workers/1,
                                    %% Middlewares
                                    [
                                    fun crud_routing:logger_middleware/1,
                                    fun crud_routing:cors_middleware/1,
                                    fun crud_routing:json_post_data_middleware/1
                                    ]),

    yaws_appmod_router:table_add_route(
                                    TableName9999,
                                    "CR", "/api/bitcoin",
                                    %% Handler
                                    fun crud_routing:handle_semaphores/1,
                                    %% Middlewares
                                    [
                                    fun crud_routing:logger_middleware/1,
                                    fun crud_routing:cors_middleware/1
                                    ]),

    setup_server(IP, {Port8080, TableName8080}, {Port9999, TableName9999}).




setup_yaws() ->
    application:load(yaws),
    application:set_env(yaws, embedded, true).

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

    SConf2 = mk_sconf("server2", IP, Port2, TableName2),

    [supervisor:start_child(yaws_sup, Ch) || Ch <- ChildSpecs],
    ok = yaws_api:setconf(GC, [SConf2 | SCList]).

mk_sconf(ServerName, IP, Port, TableName) ->
    DocRoot = "/tmp",
    #sconf{port = Port,
           listen = IP,
           servername = ServerName,
           docroot = DocRoot,
           %% NOTE: It is important to set the table name here in the opaque map,
           %% else the yaws_appmod_router will not be able to find what table to use.
           opaque = #{table_name => TableName},
           appmods = [{"/", yaws_appmod_router}]}.
