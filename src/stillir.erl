-module(stillir).

-export([set_config/2,
         set_config/3,
         set_config/4,
         get_config/2,
         get_config/3,
         update_env/3]).

-type app_name() :: atom().
-type app_key() :: atom() | [atom()].
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
    set_env_value(AppName, AppKey, EnvKey, EnvValue, Opts).

-spec get_config(app_name(), app_key()) -> app_key_value()|no_return().
get_config(AppName, AppKey) when is_atom(AppKey) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            erlang:error({missing_config, AppKey});
        {ok, Val} ->
            Val
    end;
get_config(AppName, [AppKey | SubKeys] = Key) ->
    Val = get_config(AppName, AppKey),
    case get_sub_config(SubKeys, Val) of
        undefined ->
            erlang:error({missing_config, Key});
        Value ->
            Value
    end.

-spec get_config(app_name(), app_key(), default_value()) -> app_key_value().
get_config(AppName, AppKey, DefaultValue) when is_atom(AppKey) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            DefaultValue;
        {ok, Val} ->
            Val
    end;
get_config(AppName, [AppKey | SubKeys], DefaultValue) ->
    Val = get_config(AppName, AppKey, []),
    case get_sub_config(SubKeys, Val) of
        undefined ->
            DefaultValue;
        Value ->
            Value
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

get_default(Fun) when is_function(Fun, 0) ->
    Fun();
get_default(Other) ->
    Other.

get_sub_config([], Value) ->
    Value;
get_sub_config([Key | Rest], Parent) when is_list(Parent) ->
    case proplists:get_value(Key, Parent) of
        undefined ->
            undefined;
        Value ->
            get_sub_config(Rest, Value)
    end;
get_sub_config(_, _) ->
    undefined.

set_env_value(AppName, AppKey, EnvKey, missing_env_key, Opts) ->
    case proplists:is_defined(default, Opts) of
        true ->
            DefaultValue = get_default(proplists:get_value(default, Opts)),
            set_env_value(AppName, AppKey, EnvKey, {value, DefaultValue}, Opts);
        false ->
            case proplists:get_value(required, Opts) of
                true ->
                    erlang:error({missing_env_key, {AppName, EnvKey}});
                _ ->
                    ok
            end
    end;
set_env_value(AppName, AppKey, _, {value, EnvValue}, Opts) ->
    Transform = proplists:get_value(transform, Opts),
    TransformedValue = transform_value(EnvValue, Transform),
    set_env(AppName, AppKey, TransformedValue).

set_env(AppName, AppKey, Value) when is_atom(AppKey) ->
    application:set_env(AppName, AppKey, Value);
set_env(AppName, [AppKey | SubKeys], Value) ->
    Old = application:get_env(AppName, AppKey, []),
    New = set_sub_env(SubKeys, Value, Old),
    application:set_env(AppName, AppKey, New).

set_sub_env([], Value, _) ->
    Value;
set_sub_env([Key | Rest], Value, Parent) when is_list(Parent) ->
    Old = proplists:get_value(Key, Parent, []),
    New = set_sub_env(Rest, Value, Old),
    lists:keystore(Key, 1, Parent, {Key, New});
set_sub_env([Key | Rest], Value, _) ->
    New = set_sub_env(Rest, Value, []),
    [{Key, New}].

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
