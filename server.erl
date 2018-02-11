-module(server).
%-export([start/1,stop/1,server/0]).
-compile(export_all). % DEBUG
-record(server_st, {
    server,
    nicks,
    channels
}).

% Initial state with no nicks and no channels
initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        nicks = [],
        channels = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    S = initial_state(ServerAtom),
    genserver:start(ServerAtom, S, fun server:server/2).

% Initiates and manages the channels.
server(St, {_, Channel}) ->
    {reply, ok, St}.
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    not_implemented.
