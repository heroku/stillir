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

## Setting it up

Stillir is a library but it requires you to setup an `ets` table

``` erlang
ok = stillir:init().
```

## Tests

``` erlang
$ rebar compile ct
```

## Erlang API

### Types

``` erlang
-type app_name() :: atom().
-type app_key() :: atom().
-type env_key() :: string().
-type env_var_value() :: string().
-type app_key_value() :: any().
-type default_value() :: app_key_value().
-type transform_fun() :: fun(((env_var_value())) -> app_key_value()).
-type transform() :: integer|float|binary|atom|transform_fun().
-type opt() :: {default, any()}|{transform, transform()}.
-type opts() :: [opt()]|[].
-type config_tuple() :: {app_name(), app_key(), env_key()}|
                        {app_name(), app_key(), env_key(), opts()}.
```

### Functions

``` erlang
-spec init() -> ok.
-spec set_config([config_tuple()]|[]) -> ok|no_return().
-spec set_config(app_name(), app_key(), env_key()) -> ok|no_return().
-spec get_config(app_name(), app_key()) -> app_key_value()|no_return().
-spec get_config(app_name(), app_key(), default_value()) -> app_key_value().
-spec update_env(app_name(), file:filename_all()) -> ok|no_return().
```

Note that `transform_fun/0` is not run if the default value is used.
