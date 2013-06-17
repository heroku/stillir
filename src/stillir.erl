-module(stillir).

-export([set_config/1,
         set_config/3,
         set_config/4,
         set_config/5,
         get_config/2,
         get_config/3
        ]).

-type application_name() :: atom().
-type application_key() :: atom().
-type environment_variable_key() :: string().
-type environment_variable_value() :: string().
-type application_key_value() :: any().
-type default_value() :: application_key_value().
-type transform_fun() :: fun(((environment_variable_value())) -> application_key_value()).

-spec set_config(list({application_name(), application_key(), environment_variable_key()}|
                      {application_name(), application_key(), environment_variable_key(),
                       default_value()|transform_fun()}|
                      {application_name(), application_key(), environment_variable_key(),
                       default_value(), transform_fun()})) -> ok.
set_config([]) ->
    ok;
set_config([{AppName, AppKey, EnvKey}|Rest]) ->
    set_config(AppName, AppKey, EnvKey),
    set_config(Rest);
set_config([{AppName, AppKey, EnvKey, DefaultValOrTransformFun}|Rest]) ->
    set_config(AppName, AppKey, EnvKey, DefaultValOrTransformFun),
    set_config(Rest);
set_config([{AppName, AppKey, EnvKey, DefaultValue, TransformFun}|Rest]) ->
    set_config(AppName, AppKey, EnvKey, DefaultValue, TransformFun),
    set_config(Rest).

-spec set_config(application_name(), application_key(), environment_variable_key())-> ok|no_return().
set_config(AppName, AppKey, EnvKey) ->    
    EnvValue = get_env(EnvKey),
    set_env(AppName, AppKey, EnvValue).

-spec set_config(application_name(), application_key(), environment_variable_key(),
                 default_value()|transform_fun()) -> ok|no_return.
set_config(AppName, AppKey, EnvKey, TransformFun) when is_function(TransformFun) ->
    EnvValue = get_env(EnvKey),
    TransformedValue = TransformFun(EnvValue),
    set_env(AppName, AppKey, TransformedValue);
set_config(AppName, AppKey, EnvKey, DefaultVal) ->
    EnvValue = get_env(EnvKey, DefaultVal),
    set_env(AppName, AppKey, EnvValue).

-spec get_config(application_name(), application_key()) -> application_key_value()|no_return().
get_config(AppName, AppKey) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            erlang:error({missing_config, AppKey});
        {ok, Val} ->
            Val
    end.

-spec get_config(application_name(), application_key(), default_value()) -> application_key_value().
get_config(AppName, AppKey, DefaultValue) ->
    case application:get_env(AppName, AppKey) of
        undefined ->
            DefaultValue;
        {ok, Val} ->
            Val
    end.

%% Internal
-spec set_config(application_name(), application_key(), environment_variable_key(),
                 default_value(), transform_fun()) -> ok.
set_config(AppName, AppKey, EnvKey, DefaultValue, TransformFun) when is_function(TransformFun) ->
    case get_env(EnvKey, DefaultValue) of
        DefaultValue ->
            set_env(AppName, AppKey, DefaultValue);
        EnvValue ->
            TransformedValue = TransformFun(EnvValue),
            set_env(AppName, AppKey, TransformedValue)
    end.

get_env(EnvKey) ->
    case os:getenv(EnvKey) of
        false ->
            erlang:error({missing_env_key, EnvKey});
        EnvValue ->
            EnvValue
    end.

get_env(EnvKey, DefaultValue) ->
    case os:getenv(EnvKey) of
        false ->
            DefaultValue;
        EnvValue ->
            EnvValue
    end.

set_env(AppName, AppKey, Value) ->
    application:set_env(AppName, AppKey, Value).
