-module(stproto).
-behaviour(gen_server).

%% API.
-export([start/0]).
-export([start/1]).
-export([start/2]).
-export([start_link/0]).
-export([start_link/1]).
-export([start_link/2]).
-export([stop/1]).
-export([child_spec/3]).
-export([child_spec/4]).
-export([parse/2]).
-export([parse/3]).
-export([build/3]).
-export([build/4]).
-export([build/5]).
-export([get_opts/1]).
-export([get_opts/2]).
-export([default_opts/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-type opt_val() :: <<_:8, _:_*8>>.
-type opts() :: #{
	quote => opt_val(),
	eol => opt_val(),
	vsep => opt_val(),
	asep => opt_val(),
	msep => opt_val(),
	cont => opt_val(),
	fin => opt_val()
}.
-type command_line() :: iodata().
-type parsed_command() :: #{command => binary(), variants => [binary()], arguments => [binary()]}.
-type reply_command() :: binary().
-type reply_status() :: binary().
-type reply_messages() :: undefined | binary() | [binary()].
-type reply_lines() :: [binary()].
-type ref() :: pid() | atom().

-record(state, {
	quote, eol, vsep, asep, msep, cont, fin,
	quote_size, eol_size, vsep_size, asep_size,
	quote_cps
}).

%% API.

-spec start() -> {ok, pid()}.
start() ->
	start(#{}).

-spec start(atom()) -> {ok, pid()};
           (opts()) -> {ok, pid()}.
start(Name) when is_atom(Name) ->
	start(Name, #{});
start(Opts) when is_map(Opts) ->
	Opts1=validate_opts(Opts),
	gen_server:start(?MODULE, Opts1, []).

-spec start(atom(), opts()) -> {ok, pid()}.
start(Name, Opts) ->
	Opts1=validate_opts(Opts),
	gen_server:start({local, Name}, ?MODULE, Opts1, []).

-spec start_link() -> {ok, pid()}.
start_link() ->
	start_link(#{}).

-spec start_link(atom()) -> {ok, pid()};
                (opts()) -> {ok, pid()}.
start_link(Name) when is_atom(Name) ->
	start_link(Name, #{});
start_link(Opts) when is_map(Opts) ->
	Opts1=validate_opts(Opts),
	gen_server:start_link(?MODULE, Opts1, []).

-spec start_link(atom(), opts()) -> {ok, pid()}.
start_link(Name, Opts) ->
	Opts1=validate_opts(Opts),
	gen_server:start_link({local, Name}, ?MODULE, Opts1, []).

-spec stop(ref()) -> ok.
stop(Ref) ->
	gen_server:cast(Ref, stop).

-spec child_spec(opts(), term(), permanent | transient | temporary) -> supervisor:child_spec().
child_spec(Opts, Id, Restart) ->
	#{
		id => Id,
		start => {stproto, start_link, [Opts]},
		restart => Restart,
		shutdown => brutal_kill
	}.

-spec child_spec(atom(), opts(), term(), permanent | transient | temporary) -> supervisor:child_spec().
child_spec(Name, Opts, Id, Restart) ->
	#{
		id => Id,
		start => {stproto, start_link, [Name, Opts]},
		restart => Restart,
		shutdown => brutal_kill
	}.

-spec parse(ref(), command_line()) -> {ok, parsed_command()} | {error, term()}.
parse(Ref, Str) ->
	parse(Ref, Str, 5000).

-spec parse(ref(), command_line(), timeout()) -> {ok, parsed_command()} | {error, term()}.
parse(Ref, Str, Timeout) when is_binary(Str) ->
	gen_server:call(Ref, {parse, Str}, Timeout);
parse(Ref, Str, Timeout) ->
	parse(Ref, iolist_to_binary(Str), Timeout).

-spec build(ref(), reply_command(), reply_status()) -> reply_lines().
build(Ref, Cmd, Status) ->
	build(Ref, Cmd, Status, undefined, 5000).

-spec build(ref(), reply_command(), reply_status(), reply_messages()) -> reply_lines();
           (ref(), reply_command(), reply_status(), timeout()) -> reply_lines().
build(Ref, Cmd, Status, Timeout) when is_integer(Timeout), Timeout>=0; Timeout=:=infinity ->
	build(Ref, Cmd, Status, undefined, Timeout);
build(Ref, Cmd, Status, Msgs) ->
	build(Ref, Cmd, Status, Msgs, 5000).

-spec build(ref(), reply_command(), reply_status(), reply_messages(), timeout()) -> reply_lines().
build(Ref, Cmd, Status, Msgs, Timeout) when is_binary(Cmd), is_binary(Status) ->
	gen_server:call(Ref, {build, Cmd, Status, validate_msgs(Msgs)}, Timeout).

-spec get_opts(ref()) -> opts().
get_opts(Ref) ->
	get_opts(Ref, 5000).

-spec get_opts(ref(), timeout()) -> opts().
get_opts(Ref, Timeout) ->
	gen_server:call(Ref, get_opts, Timeout).

-spec default_opts() -> opts().
default_opts() ->
	#{
		quote => <<$\\>>,
		vsep => <<$~>>,
		asep => <<$\s>>,
		msep => <<$:>>,
		cont => <<$+>>,
		fin => <<$->>,
		eol => <<$\r, $\n>>
	}.

validate_opts(Opts0) ->
	DefaultOpts=default_opts(),
	Keys=maps:keys(DefaultOpts),
	Opts1=maps:merge(DefaultOpts, Opts0),
	Opts2=maps:with(Keys, Opts1),
	lists:foreach(
		fun
			(V) when is_binary(V), byte_size(V)>0 ->
				ok;
			(_) -> error(badarg)
		end,
		maps:values(Opts2)
	),
	case Opts2 of
		%% Quote sequence must be unique
		#{quote:=V, vsep:=V} -> error(badarg);
		#{quote:=V, asep:=V} -> error(badarg);
		#{quote:=V, msep:=V} -> error(badarg);
		#{quote:=V, cont:=V} -> error(badarg);
		#{quote:=V, fin:=V} -> error(badarg);
		#{quote:=V, eol:=V} -> error(badarg);
		%% EOL sequence must be unique
		#{eol:=V, vsep:=V} -> error(badarg);
		#{eol:=V, asep:=V} -> error(badarg);
		#{eol:=V, msep:=V} -> error(badarg);
		#{eol:=V, cont:=V} -> error(badarg);
		#{eol:=V, fin:=V} -> error(badarg);
		%% Variant and Argument separator sequences must be different
		#{vsep:=V, asep:=V} -> error(badarg);
		%% Continue and final sequences must be different
		#{cont:=V, fin:=V} -> error(badarg);
		_ -> ok
	end,
	Opts2.

validate_msgs(undefined) ->
	undefined;
validate_msgs(Msg) when is_binary(Msg) ->
	Msg;
validate_msgs(Msgs) when is_list(Msgs) ->
	case lists:all(fun is_binary/1, Msgs) of
		true -> Msgs;
		false -> error(badarg)
	end;
validate_msgs(_) ->
	error(badarg).

%% gen_server.

init(Opts) ->
	Quote=maps:get(quote, Opts),
	VSep=maps:get(vsep, Opts),
	ASep=maps:get(asep, Opts),
	MSep=maps:get(msep, Opts),
	Cont=maps:get(cont, Opts),
	Fin=maps:get(fin, Opts),
	Eol=maps:get(eol, Opts),
	QuoteCps=#{
		command => binary:compile_pattern([Quote, Cont, Fin, Eol]),
		status => binary:compile_pattern([Quote, MSep, Eol]),
		message => binary:compile_pattern([Quote, Eol])
	},
	State=#state{
		quote=Quote, quote_size=byte_size(Quote), quote_cps=QuoteCps,
		eol=Eol, eol_size=byte_size(Eol),
		vsep=VSep, vsep_size=byte_size(VSep),
		asep=ASep, asep_size=byte_size(ASep),
		msep=MSep,
		cont=Cont,
		fin=Fin
	},
	{ok, State}.

handle_call(get_opts, From, State) ->
	spawn(
		fun () ->
			Opts=#{
				quote => State#state.quote,
				eol => State#state.eol,
				vsep => State#state.vsep,
				asep => State#state.asep,
				msep => State#state.msep,
				cont => State#state.cont,
				fin => State#state.fin
			},
			gen_server:reply(From, Opts)
		end
	),
	{noreply, State};
handle_call({parse, Str}, From, State) ->
	spawn(fun () -> gen_server:reply(From, do_parse(State, Str)) end),
	{noreply, State};
handle_call({build, Cmd, Status, Msgs}, From, State) ->
	spawn(fun () -> gen_server:reply(From, do_build(State, Cmd, Status, Msgs)) end),
	{noreply, State};
handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_, State) ->
	{noreply, State}.

handle_info(_, State) ->
	{noreply, State}.

terminate(_, _) ->
	ok.

code_change(_, State, _) ->
	{ok, State}.

do_parse(State, Str) ->
	do_parse(command, State, Str, #{command => <<>>, variants => [], arguments => []}).

do_parse(_, _, <<>>, _) ->
	{error, no_eol};
do_parse(_, #state{eol=Eol}, Eol, Acc=#{variants:=Vars, arguments:=Args}) ->
	{ok, Acc#{variants:=lists:reverse(Vars), arguments:=lists:reverse(Args)}};
do_parse(command, State=#state{quote=Quote, quote_size=QuoteSize, vsep=VSep, vsep_size=VSepSize, asep=ASep, asep_size=ASepSize, eol=Eol, eol_size=EolSize}, Str, Acc=#{command:=Cmd}) ->
	case Str of
		<<Quote:QuoteSize/binary, Quote:QuoteSize/binary, More/binary>> ->
			do_parse(command, State, More, Acc#{command => <<Cmd/binary, Quote/binary>>});
		<<Quote:QuoteSize/binary, ASep:ASepSize/binary, More/binary>> ->
			do_parse(command, State, More, Acc#{command => <<Cmd/binary, ASep/binary>>});
		<<Quote:QuoteSize/binary, VSep:VSepSize/binary, More/binary>> ->
			do_parse(command, State, More, Acc#{command => <<Cmd/binary, VSep/binary>>});
		<<Quote:QuoteSize/binary, Eol:EolSize/binary, More/binary>> ->
			do_parse(command, State, More, Acc#{command => <<Cmd/binary, Eol/binary>>});
		<<Quote:QuoteSize/binary, More/binary>> ->
			do_parse(command, State, More, Acc);
		<<VSep:VSepSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<>>]});
		<<ASep:ASepSize/binary, More/binary>> ->
                        do_parse(arguments, State, More, Acc#{arguments => [<<>>]});
		<<Eol:EolSize/binary, _/binary>> ->
			{error, misplaced_eol};
		<<C:1/binary, More/binary>> ->
                        do_parse(command, State, More, Acc#{command => <<Cmd/binary, C/binary>>})
	end;
do_parse(variants, State=#state{quote=Quote, quote_size=QuoteSize, vsep=VSep, vsep_size=VSepSize, asep=ASep, asep_size=ASepSize, eol=Eol, eol_size=EolSize}, Str, Acc=#{variants:=[Var|Vars]}) ->
	case Str of
		<<Quote:QuoteSize/binary, Quote:QuoteSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<Var/binary, Quote/binary>>|Vars]});
		<<Quote:QuoteSize/binary, ASep:ASepSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<Var/binary, ASep/binary>>|Vars]});
		<<Quote:QuoteSize/binary, VSep:VSepSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<Var/binary, VSep/binary>>|Vars]});
		<<Quote:QuoteSize/binary, Eol:EolSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<Var/binary, Eol/binary>>|Vars]});
		<<Quote:QuoteSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc);
		<<VSep:VSepSize/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<>>, Var|Vars]});
		<<ASep:ASepSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<>>]});
		<<Eol:EolSize/binary, _/binary>> ->
			{error, misplaced_eol};
		<<C:1/binary, More/binary>> ->
			do_parse(variants, State, More, Acc#{variants => [<<Var/binary, C/binary>>|Vars]})
	end;
do_parse(arguments, State=#state{quote=Quote, quote_size=QuoteSize, asep=ASep, asep_size=ASepSize, eol=Eol, eol_size=EolSize}, Str, Acc=#{arguments:=[Arg|Args]}) ->
	case Str of
		<<Quote:QuoteSize/binary, Quote:QuoteSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<Arg/binary, Quote/binary>>|Args]});
		<<Quote:QuoteSize/binary, ASep:ASepSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<Arg/binary, ASep/binary>>|Args]});
		<<Quote:QuoteSize/binary, Eol:EolSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<Arg/binary, Eol/binary>>|Args]});
		<<Quote:QuoteSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc);
		<<ASep:ASepSize/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<>>, Arg|Args]});
		<<Eol:EolSize/binary, _/binary>> ->
			{error, misplaced_eol};
		<<C:1/binary, More/binary>> ->
			do_parse(arguments, State, More, Acc#{arguments => [<<Arg/binary, C/binary>>|Args]})
	end.

do_build(State=#state{fin=Fin, eol=Eol}, Cmd, Status, undefined) ->
	[<<(quote(command, State, Cmd))/binary, Fin/binary, (quote(status, State, Status))/binary, Eol/binary>>];
do_build(State=#state{msep=MSep, fin=Fin, eol=Eol}, Cmd, Status, Msg) when is_binary(Msg) ->
	[<<(quote(command, State, Cmd))/binary, Fin/binary, (quote(status, State, Status))/binary, MSep/binary, (quote(message, State, Msg))/binary, Eol/binary>>];
do_build(State=#state{msep=MSep, cont=Cont, eol=Eol}, Cmd, Status, Msgs) ->
	lists:foldr(
		fun (Msg, Acc) ->
			[<<(quote(command, State, Cmd))/binary, Cont/binary, (quote(status, State, Status))/binary, MSep/binary, (quote(message, State, Msg))/binary, Eol/binary>>|Acc]
		end,
		do_build(State, Cmd, Status, undefined),
		Msgs
	).

quote(Part, #state{quote_cps=QuoteCps, quote=Quote, quote_size=QuoteSize}, Str) ->
	QuoteCp=maps:get(Part, QuoteCps),
	binary:replace(Str, QuoteCp, Quote, [global, {insert_replaced, QuoteSize}]).
