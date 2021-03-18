%%%-------------------------------------------------------------------
%%% @copyright (C) 2019, <shuwa>
%%% @doc
%%% DTU通用协议解析
%%  DTU设置
%%    1.注册包
%%    a. 在连接时发送; b.在每一数据包前加注册包
%%    注册包格式：ASCII  LOGIN 000002\r\n
%%    2.心跳
%%    a. 心跳时间设置; b.心跳数据包。
%%    心跳包格式：ASCII  HEART\r\n
%%% @end
%%% Created : 08. 十一月 2018 14:49
%%%-------------------------------------------------------------------
%%  奇迹物联
%%    AT+HELP
%%    AT+HEART=27,6805484541525468
%%    AT+LINK=680630303030303168
%%    AT+CIPSTART=TCP,132.232.121.164,21110
%%    AT+DOMAIN=13250820701.zicp.vip
%%    AT+LinkFlag1=0
%%    AT+LinkFlag2=1
%%    AT+HeartFlag=1
%%    AT+NetFlag=1
%%    AT+BPR232=2400,8,2,1
%%    AT+BPR485=9600,8,0,1

-module(dtu_decoder).
-behavior(shuwa_decoder).
-export([init/2, handle_info/2, parse_frame/2, to_frame/2]).

init(_ChannelId, State) ->
    {ok, State#{<<"auto">> => true}}.

handle_info({message, #{<<"cmd">> := <<"LOGIN">>, <<"addr">> := Addr}}, #{<<"productId">> := ProductId, <<"channelId">> := ChannelId, <<"auto">>:= Auto} = State) ->
    shuwa_bridge:send_log(ChannelId, ProductId, "DTU ~p Login", [Addr]),
    shuwa_utils:subscribe(<<"thing/", ProductId/binary, "/", Addr/binary>>),
    Auto andalso erlang:send_after(5000, self(), check),
    {ok, State#{<<"addr">> => Addr}};

handle_info({message, #{<<"cmd">> := <<"HEART">>}}, #{ <<"addr">> := Addr, <<"productId">> := ProductId, <<"channelId">> := ChannelId, <<"auto">>:= Auto} = State) ->
    shuwa_bridge:send_log(ChannelId, ProductId, "DTU ~p HEART", [Addr]),
    {ok, State};

handle_info({message, #{<<"subAddr">> := SubAddr} = Frame}, #{ <<"addr">> := Addr,  <<"channelId">> := ChannelId, <<"productId">> := ProductId} = State) ->
    Devices = maps:get(<<"devices">>, State, #{}),
    case maps:get(SubAddr, Devices, no) of
        no ->
            shuwa_bridge:send_log(ChannelId, ProductId, "DTU:~p online Recv ~s", [Addr, jsx:encode(Frame)]),
            {ok, State#{<<"devices">> => Devices#{SubAddr => true}}};
        true ->
            shuwa_bridge:send_log(ChannelId, ProductId, "DTU:~p Recv ~s", [Addr, jsx:encode(Frame)]),
            {ok, State}
    end;

handle_info(check, #{<<"send">> := Send} = State) ->
    erlang:send_after(1000, self(), next),
    {reply, Payload} = to_frame(#{<<"subAddr">> => <<"1">>}, State),
    Send(Payload),
    {ok, State};

handle_info(next, #{<<"send">> := Send} = State) ->
    erlang:send_after(5000, self(), three),
    {reply, Payload} = to_frame(#{<<"subAddr">> => <<"2">>}, State),
    Send(Payload),
    {ok, State};

handle_info(three, #{<<"send">> := Send} = State) ->
    erlang:send_after(5000, self(), check),
    {reply, Payload} = to_frame(#{<<"subAddr">> => <<"3">>}, State),
    Send(Payload),
    {ok, State};

handle_info(_Info, _State) ->
    lager:info("~p~n", [_Info]),
    ok.


parse_frame(Buff, State) ->
    parse_frame(Buff, State, []).
parse_frame(<<>>, _State, Acc) ->
    {<<>>, Acc};
parse_frame(<<"LOGIN ", Rest/binary>>, State, Acc) ->
    case parse_login(Rest) of
        {ok, Addr, Rest1} ->
            Frame = #{
                <<"cmd">> => <<"LOGIN">>,
                <<"addr">> => Addr
            },
            parse_frame(Rest1, State, Acc ++ [Frame]);
        error ->
            parse_frame(Rest, State, Acc)
    end;
parse_frame(<<"LOGIN", Rest/binary>>, State, Acc) ->
    parse_frame(<<"LOGIN ", Rest/binary>>, State, Acc);
parse_frame(<<"HEART\\r\\n", Rest/binary>>, State, Acc) ->
    Frame = #{
        <<"cmd">> => <<"HEART">>
    },
    parse_frame(Rest, State, Acc ++ [Frame]);
parse_frame(<<Addr:8, Fn:8, Len:8, Rest/binary>>, State, Acc) when Fn == 3, byte_size(Rest) >= Len + 2 ->
    <<Data:Len/binary, CRC:2/binary, Rest1/binary>> = Rest,
    case shuwa_utils:crc16(<<Addr:8, Fn:8, Len:8, Data:Len/binary>>) == CRC of
        true ->
            Frame = #{
                <<"subAddr">> => integer_to_binary(Addr),
                <<"data">> => Data
            },
            parse_userzone(Frame, Rest1, State, Acc);
        false ->
            parse_frame(Rest1, State, Acc)
    end;
parse_frame(<<_:8, Rest/binary>>, State, Acc) ->
    parse_frame(Rest, State, Acc).




%% 针对不同设备地址作不同协议解析

%% 设备为1,2 为奥松温度传感器
parse_userzone(#{ <<"subAddr">> := SubAddr, <<"data">> := Data}, Buff, State, Acc)
    when SubAddr == <<"1">>; SubAddr == <<"2">> ->
    <<Humidity:16, Temperature:16>> = Data,
    NewFrames = [#{
        <<"subAddr">> => SubAddr,
        <<"data">> => #{
            <<"temperature">> => Temperature / 10,
            <<"humidity">> => Humidity / 10
        }
    }],
    parse_frame(Buff, State, Acc ++ NewFrames);

parse_userzone(#{ <<"subAddr">> := SubAddr, <<"data">> := Data}, Buff, State, Acc) ->
    parse_frame(Buff, State, [#{
        <<"subAddr">> => SubAddr,
        <<"data">> => shuwa_utils:binary_to_hex(Data)
    } | Acc]).


%% 组装成封包, 参数为Map形式

%% 设备为1 为奥松温度传感器
to_frame(#{<<"subAddr">> := Addr}, _State) when Addr == <<"1">>; Addr == <<"2">> ->
    IntAddr = binary_to_integer(Addr),
    Bin = <<IntAddr:8, 16#03, 0:16, 2:16>>,
    CRC = shuwa_utils:crc16(Bin),
    {reply, <<Bin/binary, CRC/binary>>};

to_frame(#{<<"subAddr">> := Addr }, _State) ->
    IntAddr = binary_to_integer(Addr),
    Bin = <<IntAddr:8, 16#03, 0:16, 2:16>>,
    CRC = shuwa_utils:crc16(Bin),
    {reply, <<Bin/binary, CRC/binary>>}.


parse_login(Buff) -> parse_login(Buff, <<>>).
parse_login(<<>>, _Acc) -> error;
parse_login(<<"\\r\\n", Rest/binary>>, Acc) -> {ok, Acc, Rest};
parse_login(<<B:8, Rest/binary>>, Acc) -> parse_login(Rest, <<Acc/binary, B:8>>).
