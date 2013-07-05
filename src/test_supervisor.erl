%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(test_supervisor).

-behaviour(supervisor).

%% internal exports
-export([start_link/0,
         init/1]).



%% Start multiple supervisors only because a child can't start another
%% child before supervisor:start_child/2 has returned, although two is
%% really sufficient (listeners and monitors can be under the same).

%% start_link/2
%%
%% Callback from diameter_transport_sup as a result of start/0.
%% Starts a child supervisor under the transport supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Flags = {one_for_one, 2, 60},
    CS1 = {tag1,
                 {test_gen_server, start_link, [a0]},
                 permanent,
                 brutal_kill,
                 worker,
                 [test_gen_server]},
    CS2 = {tag2,
                 {test_gen_server, start_link, [a1]},
                 permanent,
                 brutal_kill,
                 worker,
                 [test_gen_server]},
    CS3 = {tag3,
                 {test_gen_server, start_link, [a2]},
                 permanent,
                 brutal_kill,
                 worker,
                 [test_gen_server]},
    {ok, {Flags, [CS1, CS2, CS3]}}.




