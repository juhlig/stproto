-module(stproto).

-export([parse/1, parse/2]).
-export([make_reply/2, make_reply/3]).

-record(command, {command= <<>>, variant=undefined, args=[]}).

-spec parse(iodata()) -> {binary(), undefined | binary(), [binary()]} | {error, term()}.
parse(Str) ->
	parse(Str, #{}).

-spec parse(iodata(), #{variant_separator => char(), argument_separator => char(), end_of_line => char()}) -> {binary(), undefined | binary(), [binary()]} | {error, term()};
           (iodata(), [{variant_separator, char()} | {argument_separator, char()} | {end_of_line, binary()}]) -> {binary(), undefined | binary(), [binary()]} | {error, term()}.
parse(Str, Opts) when is_binary(Str) andalso is_map(Opts) ->
	VarSep=maps:get(variant_separator, Opts, $~),
	ArgSep=maps:get(argument_separator, Opts, $\s),
	EOL=maps:get(end_of_line, Opts, <<$\r, $\n>>),
	case parse(command, Str, VarSep, ArgSep, EOL, #command{}) of
		{ok, #command{command=Command, variant=Variant, args=Args}} -> {ok, {Command, Variant, Args}};
		Other -> Other
	end;
parse(Str, Opts) when is_list(Str) ->
	parse(iolist_to_binary(Str), Opts);
parse(Str, Opts) when is_list(Opts) ->
	parse(Str, maps:from_list(Opts)).



-spec parse(command | variant | args, binary(), char(), char(), binary(), #command{}) -> {ok, #command{}} | {error, term()}.
%% no eol found -> error
parse(_, <<>>, _, _, _, _) ->
	{error, missing_eol};
%% eol when command empty -> error
parse(command, EOL, _, _, EOL, #command{command= <<>>}) ->
	{error, command};
%% eol after command -> ok
parse(command, EOL, _, _, EOL, Acc) ->
	{ok, Acc};
%% argument separator on empty command -> error
parse(command, <<ArgSep, _/binary>>, _, ArgSep, _, #command{command= <<>>}) ->
	{error, command};
%% argument separator on non-empty command -> continue with args parsing
parse(command, <<ArgSep, More/binary>>, VarSep, ArgSep, EOL, Acc) ->
	parse(args, More, VarSep, ArgSep, EOL, Acc);
%% variant separator on empty command -> error
parse(command, <<VarSep, _/binary>>, VarSep, _, _, #command{command= <<>>}) ->
	{error, command};
%% variant separator on non-empty command -> continue with variant parsing
parse(command, <<VarSep, More/binary>>, VarSep, ArgSep, EOL, Acc) ->
	parse(variant, More, VarSep, ArgSep, EOL, Acc#command{variant= <<>>});
%% uppercase command character -> lowercase and continue
parse(command, <<C, More/binary>>, VarSep, ArgSep, EOL, Acc=#command{command=Cmd}) when C>=$A andalso C=<$Z ->
	parse(command, More, VarSep, ArgSep, EOL, Acc#command{command= <<Cmd/binary, (C-$A+$a)>>});
%% non-uppercase command character -> continue
parse(command, <<C, More/binary>>, VarSep, ArgSep, EOL, Acc=#command{command=Cmd}) ->
	parse(command, More, VarSep, ArgSep, EOL, Acc#command{command= <<Cmd/binary, C>>});
%% eol after variant -> ok
parse(variant, EOL, _, _, EOL, Acc) ->
	{ok, Acc};
%% argument separator after variant -> continue with variant parsing
parse(variant, <<ArgSep, More/binary>>, VarSep, ArgSep, EOL, Acc) ->
	parse(args, More, VarSep, ArgSep, EOL, Acc);
%% uppercase variant character -> lowercase and continue
parse(variant, <<C, More/binary>>, VarSep, ArgSep, EOL, Acc=#command{variant=Var}) when C>=$A andalso C=<$Z ->
	parse(variant, More, VarSep, ArgSep, EOL, Acc#command{variant= <<Var/binary, (C-$A+$a)>>});
%% non-uppercase variant character -> continue
parse(variant, <<C, More/binary>>, VarSep, ArgSep, EOL, Acc=#command{variant=Var}) ->
	parse(variant, More, VarSep, ArgSep, EOL, Acc#command{variant= <<Var/binary, C>>});
%% argument parsing -> helper function
parse(args, Str, _, ArgSep, EOL, Acc) ->
	case parse_args(Str, ArgSep, EOL) of
		{ok, Args} -> {ok, Acc#command{args=Args}};
		Other -> Other
	end.


-spec parse_args(binary(), char(), binary()) -> {ok, [binary()]} | {error, term()}.
parse_args(Str, ArgSep, EOL) ->
	parse_args(Str, ArgSep, EOL, <<>>, []).

-spec parse_args(binary(), char(), binary(), binary(), []) -> {ok, [binary()]} | {error, term()}.
%% no eol found -> error
parse_args(<<>>, _, _, _, _) ->
	{error, missing_eol};
%% eol found, no args -> ok
parse_args(EOL, _, EOL, <<>>, []) ->
	{ok, []};
%% eol found, args -> ok
parse_args(EOL, _, EOL, ArgAcc, Acc) ->
	{ok, lists:reverse([ArgAcc|Acc])};
%% argument separator -> continue with next arg
parse_args(<<ArgSep, More/binary>>, ArgSep, EOL, ArgAcc, Acc) ->
	parse_args(More, ArgSep, EOL, <<>>, [ArgAcc|Acc]);
%% if non-terminal eol encountered -> error
%% otherwise, argument character -> continue
parse_args(Str, ArgSep, EOL, ArgAcc, Acc) ->
	EOLSize=byte_size(EOL),
	case Str of
		<<EOL:EOLSize/binary, _>> -> {error, args};
		<<C, More/binary>> -> parse_args(More, ArgSep, EOL, <<ArgAcc/binary, C>>, Acc)
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
