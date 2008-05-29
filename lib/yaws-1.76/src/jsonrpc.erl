%% Copyright (C) 2006 Gaspar Chilingarov <nm@web.am>
%%                    Gurgen Tumanyan <barbarian@armkb.com>
%% All rights reserved.
%%
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met: 
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer. 
%% 2. Redistributions in binary form must reproduce the above
%%    copyright notice, this list of conditions and the following
%%    disclaimer in the documentation and/or other materials provided
%%    with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
%% OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
%% GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(jsonrpc).
-author("Gaspar Chilingarov <nm@web.am>, Gurgen Tumanyan <barbarian@armkb.com>").
-vsn("2").
-export([call/3]).
-export([s/2]).        % extract element from proplist

%%%
%%% call function calls json-rpc method on remote host 
%%%
%%% URL - remote server url (may use https)
%%% Options - option list to be passed to http:request 
%% (ssl options ot timeout, for example)
%%% Payload -> {call, MethodName, Args} tuple
%%% MethodName -> atom
%%% Args -> list
%%%
call(URL, Options, Payload) -> 
    try
        {ok, CallPayloadDeep} = encode_call_payload(Payload),
        CallPayload = lists:flatten(CallPayloadDeep),
        {ok, Response} = http:request(post, 
            {URL,[{"Content-length",length(CallPayload)}],
             "application/x-www-form-urlencoded",CallPayload}, 
                                      Options, []),

        RespBody= if (size(Response) == 2) or (size(Response) == 3) -> 
                          element(size(Response), Response) 
                  end,
        decode_call_payload(RespBody)
    catch
        error:Err-> 
            error_logger:error_report([{'json_rpc:call', error}, 
                                       {error, Err}, 
                                       {stack, erlang:get_stacktrace()}]),
            {error,Err}
    end. 

%%%
%%% json-rpc.org defines such structure for making call
%%% 
%%% {"method":"methodname", "params": object, "id": integer}
encode_call_payload({call, Method, Args}) when is_atom(Method) and 
                                               is_list(Args) ->  
    ID = element(3, erlang:now()), % id makes sense when there are many 
                                                % requests in same
                                                % communication channel and 
                                                %replies can come in random 
                                                %order here it can be changed 
                                                %to something less expensive
    Struct =  json:encode({struct, [{method, atom_to_list(Method)}, 
                                    {params, {array, Args}}, 
                                    {id, ID}]}),
    {ok, Struct}. 
     
%%%
%%% decode response structure
%%% 
%%% {"id":requestID,"result":object,"error":error_description}
decode_call_payload(JSonStr) -> 
    {ok, JSON} = json:decode_string(JSonStr),
    Result = s(JSON, result),
    Error = s(JSON, error),
%    ID = s(JSON, id),    % ignored for now
    if 
        (Error =/= undefined) -> 
            {error, Error};
        true -> 
            {ok,{response,[Result]}} % make it compliant with xmlrpc response
    end. 
    
%%% lookup element in proplist 
%%% XXX: are there ready implementation in erlang std library?
s ({struct, List}, ElemName) -> 
    s(List, ElemName);

s(List, ElemName) when is_list(List) ->
    case lists:keysearch(ElemName,1,List) of
    {value,{ElemName,Val}} ->
        Val;
    _ ->
        undefined
    end. 

    
% vim: tabstop=4 ft=erlang
