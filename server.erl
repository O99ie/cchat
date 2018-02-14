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
channel_initial_state(ChannelAtom, PidAtom) -> 
    #channel_st{
        channel = ChannelAtom, 
        pids  = [PidAtom]
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
    case lists:member(Channel, St#server_st.channels) of
        false ->
            S  = channel_initial_state(Channel, Pid),
            genserver:start(CH, S, fun server:channel/2),
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
    {reply, ok, St};

% Receive message from client
server(St, {message_send, Pid, Nick, Channel, Msg}) ->
    CH = list_to_atom(Channel),
    genserver:request(CH, {message_send, Pid, Nick, Msg}),
    {reply, ok, St}.


% Handles the activty on the channels.
% Joins channel.
channel(St, {join, Pid}) ->
    {reply, ok, St#channel_st{pids = [Pid | St#channel_st.pids]}};

% Leaves channel.
channel(St, {leave, Pid}) ->
        S = St#channel_st{pids = lists:delete(Pid, St#channel_st.pids)},
        case St#channel_st.pids == [] of
            true -> {reply, genserver:stop(St#channel_st.channel), S};
            false -> {reply, ok, S}
        end;

channel(St, {message_send, Pid, Nick, Msg}) ->
    %whereis(Pid),
    genserver:request((list_to_atom(pid_to_list(hd(St#channel_st.pids)))), {message_receive, St#channel_st.channel, Nick, Msg}),
    
    
    %[genserver:request(list_to_atom(pid_to_list(P)),
    % {message_receive, St#channel_st.channel, Nick, Msg})
    %    || P <- St#channel_st.pids, P =/= Pid],
    {reply, ok, St}.
    

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

% join()
