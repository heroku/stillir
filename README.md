# Stillir

A library to cache Environmental variables for your Erlang application, useful when you 
pass configuration details to your application using the environment.

Reason: http://www.12factor.net/config

[![Build Status](https://travis-ci.org/heroku/stillir.svg?branch=master)](https://travis-ci.org/heroku/stillir)

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
-type default_fun() :: fun(() -> default_value()).
-type transform() :: integer|float|binary|atom|transform_fun().
-type opt() :: {default, any()|default_fun()}|{transform, transform()}|required.
-type opts() :: [opt()]|[].
-type config_spec() :: {app_key(), env_key()}|
                       {app_key(), env_key(), opts()}.
-type config_specs() :: [config_spec()].
```

### Functions

``` erlang
-spec set_config(app_name(), config_specs()|[]) -> ok|no_return().
-spec set_config(app_name(), app_key(), env_key()) -> ok|no_return().
-spec get_config(app_name(), app_key()) -> app_key_value()|no_return().
-spec get_config(app_name(), app_key(), default_value()) -> app_key_value().
-spec update_env(app_name(), file:filename_all(), config_specs()|[]) -> ok|no_return().
```

The `transform_fun/0` is only run if the input is a list.
