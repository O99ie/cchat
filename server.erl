-module(server).
%-export([start/1,stop/1,server/0]).
-compile(export_all). % DEBUG
-record(server_st, {
    server,
    nicks,
    channels
}).

% Initial state with one nick and one channel
initial_state(ServerAtom, Nick, Channel) ->
    #server_st{
        server = ServerAtom,
        nicks = [Nick],
        channels = [Channel]
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    start(ServerAtom, nick, channel).
    

start(ServerAtom, Nick, Channel) ->
    S = initial_state(ServerAtom, Nick, Channel),
    genserver:start(ServerAtom, S, fun server:server/2).

server(St, {_, Channel}) ->
    {reply, ok, St}.
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
