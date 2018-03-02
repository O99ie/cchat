-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    initNick, % the original nick of the client
    server, % atom of the chat server
    channels, % atom of the channel joined
    pid % pid of client process
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        initNick = list_to_atom(Nick),
        server = ServerAtom,
        channels = []
    }.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client

% Join channel.
handle(St, {join, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of	% Check: Has the client already joined the channel?
        false ->
            try genserver:request(St#client_st.server, {join, St#client_st.initNick, Channel}) of
                _ -> NewState = St#client_st{channels = [Channel | St#client_st.channels]},
                     {reply, ok, NewState}
            catch
                error:_ -> {reply, {error, server_not_reached, "Server not reached"}, St}
            end;
        true ->
            {reply, {error, user_already_joined, "User already joined to channel"}, St}
    end;

% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of	% Check: Is the client even a member of the channel?
        true ->
            genserver:request(St#client_st.server, {leave, St#client_st.initNick, Channel}),
            {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}};
        false ->
            {reply, {error, user_not_joined, "User has not joined the channel they are trying to leave"}, St}
    end;
    

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel, St#client_st.channels) of
        true ->
            ChannelAtom = list_to_atom(Channel),
            genserver:request(ChannelAtom,
                {message_send, St#client_st.initNick, St#client_st.nick, Msg}),
            {reply, ok, St};
        false ->
            {reply, {error, user_not_joined,
            "User has not joined the channel they are trying to message"}, St}
    end;
            

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .
