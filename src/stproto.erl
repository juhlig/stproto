-module(stproto).

-export([parse/1]).
-export([make_reply/2, make_reply/3]).

-record(command, {command= <<>>, variant=undefined, args=[]}).

-spec parse(iodata()) -> {ok, {binary(), undefined | binary(), [binary()]}} | {error, term()}.
parse(Str) when is_binary(Str) ->
	case parse(command, Str, #command{}) of
		{ok, #command{command=Command, variant=Variant, args=Args}} -> {ok, {Command, Variant, Args}};
		Other -> Other
	end;
parse(Str) when is_list(Str) ->
	parse(iolist_to_binary(Str)).



-spec parse(command | variant | args, binary(), #command{}) -> {ok, #command{}} | {error, term()}.
%% no eol found -> error
parse(_, <<>>, _) ->
	{error, missing_eol};
%% eol when command empty -> error
parse(command, <<$\r, $\n>>, #command{command= <<>>}) ->
	{error, command};
%% eol after command -> ok
parse(command, <<$\r, $\n>>, Acc) ->
	{ok, Acc};
%% argument separator on empty command -> error
parse(command, <<$\s, _/binary>>, #command{command= <<>>}) ->
	{error, command};
%% argument separator on non-empty command -> continue with args parsing
parse(command, <<$\s, More/binary>>, Acc) ->
	parse(args, More, Acc);
%% variant separator on empty command -> error
parse(command, <<$~, _/binary>>, #command{command= <<>>}) ->
	{error, command};
%% variant separator on non-empty command -> continue with variant parsing
parse(command, <<$~, More/binary>>, Acc) ->
	parse(variant, More, Acc#command{variant= <<>>});
%% uppercase command character -> lowercase and continue
parse(command, <<C, More/binary>>, Acc=#command{command=Cmd}) when C>=$A andalso C=<$Z ->
	parse(command, More, Acc#command{command= <<Cmd/binary, (C-$A+$a)>>});
%% non-uppercase command character -> continue
parse(command, <<C, More/binary>>, Acc=#command{command=Cmd}) ->
	parse(command, More, Acc#command{command= <<Cmd/binary, C>>});
%% eol after variant -> ok
parse(variant, <<$\r, $\n>>, Acc) ->
	{ok, Acc};
%% argument separator after variant -> continue with variant parsing
parse(variant, <<$\s, More/binary>>, Acc) ->
	parse(args, More, Acc);
%% uppercase variant character -> lowercase and continue
parse(variant, <<C, More/binary>>, Acc=#command{variant=Var}) when C>=$A andalso C=<$Z ->
	parse(variant, More, Acc#command{variant= <<Var/binary, (C-$A+$a)>>});
%% non-uppercase variant character -> continue
parse(variant, <<C, More/binary>>, Acc=#command{variant=Var}) ->
	parse(variant, More, Acc#command{variant= <<Var/binary, C>>});
%% argument parsing -> helper function
parse(args, Str, Acc) ->
	case parse_args(Str) of
		{ok, Args} -> {ok, Acc#command{args=Args}};
		Other -> Other
	end.


-spec parse_args(binary()) -> {ok, [binary()]} | {error, term()}.
parse_args(Str) ->
	parse_args(Str, <<>>, []).

-spec parse_args(binary(), binary(), [binary()]) -> {ok, [binary()]} | {error, term()}.
%% no eol found -> error
parse_args(<<>>, _, _) ->
	{error, missing_eol};
%% eol found, no args -> ok
parse_args(<<$\r, $\n>>, <<>>, []) ->
	{ok, []};
%% eol found, args -> ok
parse_args(<<$\r, $\n>>, ArgAcc, Acc) ->
	{ok, lists:reverse([ArgAcc|Acc])};
%% argument separator -> continue with next arg
parse_args(<<$\s, More/binary>>, ArgAcc, Acc) ->
	parse_args(More, <<>>, [ArgAcc|Acc]);
%% if non-terminal eol encountered -> error
%% otherwise, argument character -> continue
parse_args(Str, ArgAcc, Acc) ->
	case Str of
		<<$\r, $\n, _>> -> {error, args};
		<<C, More/binary>> -> parse_args(More, <<ArgAcc/binary, C>>, Acc)
	end.



-spec make_reply(binary(), ok | error | binary()) -> [binary()].
make_reply(Cmd, Status) ->
	make_reply1(Cmd, Status, undefined).

-spec make_reply(binary(), ok | error | binary(), undefined | binary() | [binary()]) -> [binary()].
make_reply(Cmd, Status, undefined) ->
	make_reply1(Cmd, Status, []);

make_reply(Cmd, Status, Msgs) ->
	make_reply1(Cmd, Status, Msgs).

make_reply1(Cmd, Status, Msgs) ->
	make_reply2(string:uppercase(Cmd), Status, Msgs).

make_reply2(Cmd, ok, Msgs) ->
	make_reply3(Cmd, <<"OK">>, Msgs);

make_reply2(Cmd, error, Msgs) ->
	make_reply3(Cmd, <<"ERROR">>, Msgs);

make_reply2(Cmd, Custom, Msgs) when is_binary(Custom) ->
	make_reply3(Cmd, string:uppercase(Custom), Msgs).

make_reply3(Cmd, StatusStr, Msgs) when is_list(Msgs) ->
	lists:reverse([<<Cmd/binary, $-, StatusStr/binary>> | [<<Cmd/binary, $+, StatusStr/binary, $:, Msg/binary>> || Msg <- Msgs]]);

make_reply3(Cmd, StatusStr, Msg) when is_binary(Msg) ->
	[<<Cmd/binary, $-, StatusStr/binary, $:, Msg/binary>>];

make_reply3(Cmd, StatusStr, undefined) ->
	[<<Cmd/binary, $-, StatusStr/binary>>].
