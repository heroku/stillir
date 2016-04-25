-module(stillir).

-export([set_config/2,
         set_config/3,
         set_config/4,
         get_config/2,
         get_config/3,
         update_env/3]).

-type app_name() :: atom().
-type app_key() :: atom().
-type env_key() :: string().
-type env_var_value() :: string().
-type app_key_value() :: any().
-type default_value() :: app_key_value().
-type transform_fun() :: fun(((env_var_value())) -> app_key_value()).
-type default_fun() :: fun(() -> default_value()).
-type transform() :: integer|float|binary|atom|transform_fun().
-type opt() :: {default, any()|default_fun()}|{transform, transform()}|required.
-type opts() :: [opt()]|[].
-type config_spec() :: {app_key(), env_key()}|
                       {app_key(), env_key(), opts()}.
-type config_specs() :: [config_spec()].

-spec set_config(app_name(), config_specs()|[]) -> ok|no_return().
set_config(_, []) ->
    ok;
set_config(AppName, [{AppKey, EnvKey}|Rest]) ->
    set_config(AppName, AppKey, EnvKey),
    set_config(AppName, Rest);
set_config(AppName, [{AppKey, EnvKey, Opts}|Rest]) ->
    set_config(AppName, AppKey, EnvKey, Opts),
    set_config(AppName, Rest).

-spec set_config(app_name(), app_key(), env_key()) -> ok|no_return().
set_config(AppName, AppKey, EnvKey) ->
    set_config(AppName, AppKey, EnvKey, []).

-spec set_config(app_name(), app_key(), env_key(),
                 opts()) -> ok|no_return().
set_config(AppName, AppKey, EnvKey, Opts) ->
    EnvValue = get_env(EnvKey),
    AppEnvValue = get_app_env(AppName, AppKey),
    set_env_value(AppName, AppKey, EnvKey, EnvValue, AppEnvValue, Opts).

-spec get_config(app_name(), app_key()) -> app_key_value()|no_return().
get_config(AppName, AppKey) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            erlang:error({missing_config, AppKey});
        {ok, Val} ->
            Val
    end.

-spec get_config(app_name(), app_key(), default_value()) -> app_key_value().
get_config(AppName, AppKey, DefaultValue) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            DefaultValue;
        {ok, Val} ->
            Val
    end.

-spec update_env(app_name(), file:filename_all(),
                 config_specs()|[]) -> ok|no_return().
update_env(AppName, Filename, Specs) ->
    case file:open(Filename, [read, raw]) of
        {ok, IoDev} ->
            NewValues = read_file(IoDev, []),
            reread_environment(AppName, NewValues, Specs);
        {error, _Error} = Error ->
            Error
    end.

%% Internal
transform_value(Value, undefined) when is_list(Value) ->
    Value;
transform_value(Value, integer) when is_list(Value) ->
    list_to_integer(Value);
transform_value(Value, float) when is_list(Value) ->
    list_to_float(Value);
transform_value(Value, binary) when is_list(Value) ->
    list_to_binary(Value);
transform_value(Value, atom) when is_list(Value) ->
    list_to_atom(Value);
transform_value(Value, boolean) when is_list(Value) ->
    case string:to_lower(string:strip(Value)) of
        "true" ->
            true;
        "yes" ->
            true;
        "1" ->
            true;
        "false" ->
            false;
        "no" ->
            false;
        "0" ->
            false;
        _ ->
            erlang:error({not_a_boolean, Value})
    end;
transform_value(Value, Fun) when is_function(Fun, 1) andalso is_list(Value) ->
    Fun(Value);
transform_value(Value, _) ->
    Value.

reread_environment(_, _, []) ->
    ok;
reread_environment(AppName, NewValues, [{AppKey, EnvKey}|Rest]) ->
    set_config(AppName, AppKey, {EnvKey, NewValues}),
    reread_environment(AppName, NewValues, Rest);
reread_environment(AppName, NewValues, [{AppKey, EnvKey, Opts}|Rest]) ->
    set_config(AppName, AppKey, {EnvKey, NewValues}, Opts),
    reread_environment(AppName, NewValues, Rest).

get_env({EnvKey, EnvList}) ->
    case proplists:get_value(EnvKey, EnvList) of
        undefined ->
            missing_env_key;
        EnvValue ->
            {value, EnvValue}
    end;
get_env(EnvKey) ->
    case os:getenv(EnvKey) of
        false ->
            missing_env_key;
        EnvValue ->
            {value, EnvValue}
    end.

get_app_env(AppName, AppKey) ->
    case application:get_env(AppName, AppKey) of
        {ok, Value} ->
            {value, Value};
        undefined ->
            missing_app_env_key
    end.

get_default(Fun) when is_function(Fun, 0) ->
    Fun();
get_default(Other) ->
    Other.

set_env_value(AppName, AppKey, EnvKey, missing_env_key, missing_app_env_key, Opts) ->
    case proplists:is_defined(default, Opts) of
        true ->
            DefaultValue = get_default(proplists:get_value(default, Opts)),
            set_env_value(AppName, AppKey, EnvKey, {value, DefaultValue}, missing_app_env_key, Opts);
        false ->
            case proplists:get_value(required, Opts) of
                true ->
                    erlang:error({missing_env_key, {AppName, EnvKey}});
                _ ->
                    ok
            end
    end;
set_env_value(AppName, AppKey, _, {value, EnvValue}, _, Opts) ->
    Transform = proplists:get_value(transform, Opts),
    TransformedValue = transform_value(EnvValue, Transform),
    set_env(AppName, AppKey, TransformedValue);
set_env_value(AppName, AppKey, _, _, {value, AppEnvValue}, _) ->
    set_env(AppName, AppKey, AppEnvValue).

set_env(AppName, AppKey, Value) ->
    application:set_env(AppName, AppKey, Value).

read_file(IoDev, Retval) ->
    case file:read_line(IoDev) of
        {ok, Data} ->
            Res = handle_line(Data),
            read_file(IoDev, Retval ++ [Res]);
        eof ->
            Retval;
        {error, _Error} = Error ->
            Error
    end.

handle_line(Data) ->
    case re:split(Data, "^export\s+([A-Z0-9_]+)='?([^']*)'?\n$") of
        [_, EnvKey, EnvVar, _] ->
            {binary_to_list(EnvKey), binary_to_list(EnvVar)};
        Error ->
            error_logger:info_msg("app=stillir at=handle_line error=~p", [Error]),
            no_match
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

transform_value_test_() ->
    [
     ?_assertEqual(42, transform_value("42", integer))
    ,?_assertEqual(-1, transform_value("-1", integer))
    ,?_assertError(badarg, transform_value("hello", integer))
    ,?_assertError(badarg, transform_value("1.5", integer))

    ,?_assertEqual(42.0, transform_value("42.0", float))
    ,?_assertEqual(4.2, transform_value("4.2", float))
    ,?_assertError(badarg, transform_value("hello", float))
    ,?_assertError(badarg, transform_value("42", float))
    ,?_assertError(badarg, transform_value("42.", float))

    ,?_assertEqual(<<"hello">>, transform_value("hello", binary))
    ,?_assertEqual(<<"5">>, transform_value("5", binary))
    ,?_assertEqual(<<>>, transform_value("", binary))

    ,?_assertEqual(hello, transform_value("hello", atom))
    ,?_assertEqual('5', transform_value("5", atom))
    ,?_assertEqual('', transform_value("", atom))

    ,?_assertEqual(true, transform_value("true", boolean))
    ,?_assertEqual(true, transform_value("TRUE", boolean))
    ,?_assertEqual(true, transform_value("1", boolean))
    ,?_assertEqual(false, transform_value("NO", boolean))
    ,?_assertEqual(false, transform_value("0", boolean))
    ,?_assertEqual(false, transform_value("False", boolean))
    ,?_assertError({not_a_boolean, "foo"}, transform_value("foo", boolean))

    ,?_assertEqual(strawberry, transform_value("1024", fun (_) -> strawberry end))

    ,?_assertEqual("40", transform_value("40", complex_number))

    ,?_assertEqual("40", transform_value("40", undefined))
    ].

-endif.
