# Stillir

A library to cache Environmental variables for your Erlang application, useful when you 
pass configuration details to your application using the environment.

Reason: http://www.12factor.net/configw

## The name

Stillir is icelandic for "the one who configures", it can also be used as a verb:

```
Hann stillir kaffikönnuna
```

Translates to

```
He configures the coffee machine.
```

## Tests

``` erlang
$ rebar ct
```

## Erlang API

``` erlang
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
-spec get_config(application_name(), application_key(), default_value()) -> application_key_value().
```

Note that `transform_fun/0` is not run if the default value is used.
