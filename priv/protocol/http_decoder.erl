%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2020
%%% @doc
%%% HTTP 编码器例子
%%% @end
%%%-------------------------------------------------------------------
-module(http_decoder).
-author("kenneth").
-behavior(shuwa_decoder).
-export([handle_info/2]).


handle_info({http, Req}, State) ->
    Path = shuwa_req:path(Req),
    Method = shuwa_req:method(Req),
    lager:info("~p ~p", [Method, Path]),
    handle_info(Path, Req, State).


handle_info(<<"/test">>, _Req, _State) ->
    {reply, {200, <<"Hello World">>}};

handle_info(<<"/test1">>, _Req, State) ->
    Header = #{
        <<"content-type">> => <<"application/json; charset=utf-8">>
    },
    {reply, {400, Header, <<"Forbidden">>}, State}.
