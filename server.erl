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

% Initiates and manages the channels.
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

server(St, {leave, Pid, Channel}) ->
    CH = list_to_atom(Channel),
    genserver:request(CH, {leave, Pid}), % Kolla om kanalen finns först
    {reply, ok, St}.
    

% Handles the activty on the channels
channel(St, {join, Pid}) ->
    {reply, ok, St#channel_st{pids = [Pid | St#channel_st.pids]}};
channel(St, {leave, Pid}) ->
    case lists:member(Pid, St#channel_st.pids) of
        true ->
            S = St#channel_st{pids = lists:delete(Pid, St#channel_st.pids)},
            case St#channel_st.pids == [] of
                true -> {reply, genserver:stop(St#channel_st.channel), S};
                false -> {reply, ok, S}
            end;
        false ->
            {reply, {error, user_not_joined, "User not joined"}, St}
    end.

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    genserver:stop(ServerAtom).

% join()
