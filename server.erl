-module(server).
%-export([start/1,stop/1,server/0]).
-compile(export_all). % DEBUG

-record(server_st, {
    server,
    channels
}).
-record(channel_st, {
    channel,
    pids
}).

% Initial state with no nicks and no channels
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
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    % - Register this process to ServerAtom
    % - Return the process ID
    S = server_initial_state(ServerAtom),
    genserver:start(ServerAtom, S, fun server:server/2).

% "Server" initiates and manages the channels.
% Join channel.
server(St, {join, Pid, Channel}) ->
    CH = list_to_atom(Channel),
    case lists:member(CH, St#server_st.channels) of
        false ->
            io:fwrite(pid_to_list(Pid)++" is joining channel "++Channel),   %DEBUG
            S  = channel_initial_state(Channel),
            io:fwrite(pid_to_list(genserver:start(CH, S, fun server:channel/2))++" is a new channel"),
            genserver:request(CH, {join, Pid}),
            {reply, ok, St#server_st{channels = [CH | St#server_st.channels]}};
        true ->
            case genserver:request(CH, {join, Pid}) of
                ok -> {reply, ok, St}
            end
    end;

% Leave channel.
server(St, {leave, Pid, Channel}) ->
    CH = list_to_atom(Channel),
    genserver:request(CH, {leave, Pid}),
    {reply, ok, St}.


% Handles the activty on the channels.
% Joins channel.
channel(St, {join, Pid}) ->
    Pids = St#channel_st.pids,
    S = St#channel_st{pids = [Pid | Pids]},
    io:fwrite("All pids joined to channel "++S#channel_st.channel++":~n"), %DEBUG
    [io:fwrite(pid_to_list(X)++"~n") || X <- S#channel_st.pids],   %DEBUG
    {reply, ok, S};

% Leaves channel.
channel(St, {leave, Pid}) ->
        S = St#channel_st{pids = lists:delete(Pid, St#channel_st.pids)},
        case St#channel_st.pids == [] of
            true -> {reply, genserver:stop(St#channel_st.channel), S};
            false -> {reply, ok, S}
        end;

channel(St, {message_send, Pid, Nick, Msg}) ->
    %whereis(Pid),
    %genserver:request((list_to_atom(pid_to_list(hd(St#channel_st.pids)))), {message_receive, St#channel_st.channel, Nick, Msg}),
    
    
    %[genserver:request(list_to_atom(pid_to_list(P)),
    %{message_receive, St#channel_st.channel, Nick, Msg})
    %    || P <- St#channel_st.pids, P =/= Pid],
    
    Ls = lists:delete(Pid, St#channel_st.pids),

    [genserver:request(p2a(X), msg(St, Nick, Msg)) || X <- Ls],
    
    {reply, ok, St}.

p2l(X) -> pid_to_list(X).
l2a(X) -> list_to_atom(X).
p2a(X) -> l2a(p2l(X)).
msg(St, Nick, Msg) -> {message_receive, St#channel_st.channel, Nick, Msg}.
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

% join()
