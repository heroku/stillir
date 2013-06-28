-module(stillir_first_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom. Groups are named as {group, GroupName}. The tests
%% will run in the order given in the list.
all() ->
    [set_conf_no_default
     ,set_conf_default
     ,set_conf_transform_fun
     ,set_conf_default_transform_fun
     ,set_conf_default_transform_fun2
     ,set_conf_list
     ,get_conf
     ,update_env
    ].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(set_conf_no_default, Config) ->
    true = os:putenv("SET_CONF_NO_DEFAULT", "test value 1"),
    Config;
init_per_testcase(set_conf_transform_fun, Config) ->
    true = os:putenv("SET_CONF_TRANSFORM_FUN", "1000"),
    Config;
init_per_testcase(set_conf_default_transform_fun, Config) ->
    true = os:putenv("SET_CONF_DEFAULT_TRANSFORM_FUN", "atom"),
    Config;
init_per_testcase(set_conf_list, Config) ->
    true = os:putenv("SET_CONF_LIST1", "one"),
    true = os:putenv("SET_CONF_LIST2", "two"),
    true = os:putenv("SET_CONF_LIST3", "10.01"),
    Config;
init_per_testcase(get_conf, Config) ->
    application:set_env(stillir, foo, bar),
    Config;    
init_per_testcase(update_env, Config) ->
    lists:foreach(fun({Key, Var}) ->
                          true = os:putenv(Key, Var)
                  end, [{"CONF1", "var1"},
                        {"CONF2", "var2"},
                        {"CONF3", "var3"}]),
    Config;
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
set_conf_no_default(Config) ->
    ok = stillir:set_config(stillir, set_conf_no_default, "SET_CONF_NO_DEFAULT"),
    "test value 1" = stillir:get_config(stillir, set_conf_no_default),
    try stillir:set_config(stillir, set_conf_no_default_fail, "SET_CONF_NO_DEFAULT_FAIL", [required]) of
        _ ->
            throw({error, got_unavailable_env})
    catch
        error:{missing_env_key, {stillir, "SET_CONF_NO_DEFAULT_FAIL"}} ->
            ok
    end,
    Config.

set_conf_default(Config) ->
    ok = stillir:set_config(stillir, set_conf_default, "SET_CONF_DEFAULT", [{default, default_value}]),
    default_value = stillir:get_config(stillir, set_conf_default),    
    Config.

set_conf_transform_fun(Config) ->
    ok = stillir:set_config(stillir, set_conf_transform_fun, "SET_CONF_TRANSFORM_FUN",
                            [{transform, integer}]),
    1000 = stillir:get_config(stillir, set_conf_transform_fun),
    Config.

set_conf_default_transform_fun(Config) ->
    ok = stillir:set_config(stillir, set_conf_default_transform_fun, "SET_CONF_DEFAULT_TRANSFORM_FUN",
                            [{transform, atom},
                             {default, unused_default}]),
    atom = stillir:get_config(stillir, set_conf_default_transform_fun),
    Config.

set_conf_default_transform_fun2(Config) ->
    ok = stillir:set_config(stillir, set_conf_default_transform_fun2, "SET_CONF_DEFAULT_TRANSFORM_FUN2_FAIL",
                            [{default, default_value},
                             {transform, fun(_L) -> erlang:error(transform_fun_run) end}]),
    default_value = stillir:get_config(stillir, set_conf_default_transform_fun2),
    Config.

set_conf_list(Config) ->
    ok = stillir:set_config(stillir, [{set_config_list1, "SET_CONF_LIST1"},
                                      {set_config_list2, "SET_CONF_LIST_FAIL1", [{default, default1}]},
                                      {set_config_list3, "SET_CONF_LIST3", [{transform, float}]},
                                      {set_config_list4, "SET_CONF_LIST_FAIL2", [{default, default2},
                                                                                 {transform, float}]},
                                      {set_config_list5, "SET_CONF_LIST2", [{default, unused},
                                                                            {transform, fun(L) -> list_to_atom(L) end}]}]),
    "one" = stillir:get_config(stillir, set_config_list1),
    default1 = stillir:get_config(stillir, set_config_list2),
    10.01 = stillir:get_config(stillir, set_config_list3),
    default2 = stillir:get_config(stillir, set_config_list4),
    two = stillir:get_config(stillir, set_config_list5),
    Config.

get_conf(Config) ->
    bar = stillir:get_config(stillir, foo),
    default_val = stillir:get_config(stillir, dingo, default_val),
    Config.

update_env(Config) ->
    Specs = [{update_1, "CONF1"},
             {update_2, "CONF2", [{transform, atom}]},
             {update_3, "CONF3", [{transform, atom}]}],
    ok = stillir:set_config(stillir, Specs),
    "var1" = stillir:get_config(stillir, update_1),
    var2 = stillir:get_config(stillir, update_2),
    var3 = stillir:get_config(stillir, update_3),
    stillir:update_env(stillir, code:lib_dir(stillir) ++ "/test/keys.sh", Specs),
    "updated_var1" = stillir:get_config(stillir, update_1),
    updated_var2 = stillir:get_config(stillir, update_2),
    updated_var3 = stillir:get_config(stillir, update_3),
    Config.
