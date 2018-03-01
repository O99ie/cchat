-module(server).
-export([start/1,stop/1,server/2,channel/2]).

-record(server_st, {
    server,
    channels
}).
-record(channel_st, {
    channel,
    pids
}).

% Initial states with no nicks and no channels
server_initial_state(ServerAtom) ->
    #server_st{
        server = ServerAtom,
        channels = []
    }.
channel_initial_state(ChannelAtom) -> 
    #channel_st{
        channel = ChannelAtom, 
        pids  = []
    }.

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    S = server_initial_state(ServerAtom),
    genserver:start(ServerAtom, S, fun server:server/2).

% "server" initiates and manages the channels.
% Join channel.
server(St, {join, Pid, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    case lists:member(ChannelAtom, St#server_st.channels) of    % Check: Does the channel already exist?
        false ->
            S  = channel_initial_state(ChannelAtom),
            genserver:start(ChannelAtom, S, fun server:channel/2),
            genserver:request(ChannelAtom, {join, Pid}),
            NewChannels = [ChannelAtom | St#server_st.channels],
            {reply, ok, St#server_st{channels = NewChannels}};
        true ->
            genserver:request(ChannelAtom, {join, Pid}),
            {reply, ok, St}
    end;

% Leave channel.
server(St, {leave, Pid, Channel}) ->
    ChannelAtom = list_to_atom(Channel),
    genserver:request(ChannelAtom, {leave, Pid}),
    {reply, ok, St}.

% "channel" handles the activty on the channels.
% Joins channel.
channel(St, {join, Pid}) ->
    NewPids = [Pid | St#channel_st.pids],
    {reply, ok, St#channel_st{pids = NewPids}};

% Leaves channel.
channel(St, {leave, Pid}) ->
        NewState = St#channel_st{pids = lists:delete(Pid, St#channel_st.pids)},
        {reply, ok, NewState};

channel(St, {message_send, Pid, Nick, Msg}) ->
    Recipients = lists:delete(Pid, St#channel_st.pids),
    Reply = {message_receive, atom_to_list(St#channel_st.channel), Nick, Msg},
    spawn(fun() -> [spawn((fun() -> genserver:request(p2a(X), Reply) end)) || X <- Recipients] end),
    {reply, ok, St}.

p2a(X) -> list_to_atom(pid_to_list(X)). 

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).

