%% Copyright (C) 2013 Evax Software <contact@evax.fr>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
%% @doc A simple Erlang XMLRPC client
-module(xmlrpcc).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([start/0, call/3, call/4]).

%% @doc Helper to start the application in the console.
start() ->
   hackney:start(),
   application:start(xmlrpcc).

%% @doc XMLRPC call.
%% @equiv call(Url, Method, Args, [])
call(Url, Method, Args) ->
    call(Url, Method, Args, []).

%% @doc XMLRPC call.
%% The Opts argument is passed to hackney
call(Url, Method, Args, Opts) ->
   case hackney:request(post, Url,
                        [{<<"Content-Type">>, <<"text/xml">>}],
                        xmlrequest(Method, Args), Opts) of
      {error, Reason} ->
         {error, Reason};
      {ok, 200, _, Client} ->
         {ok, Body, _} = hackney:body(Client),
         [Resp] = get_tags(Body, methodResponse),
         case get_tags(Resp, fault) of
            [] ->
               [Params] = get_tags(Resp, params),
               [Param] = get_tags(Params, param),
               [Value] = get_tags(Param, value),
               {ok, decode(Value)};
            [Fault] ->
               [Value] = get_tags(Fault, value),
               {error, decode(Value)}
         end
   end.

%% Private functions

xmlrequest(Method, Args) ->
   M = coerce_binary(Method),
   Ps = encode_params(Args),
   <<"<?xml version=\"1.0\"?><methodCall><methodName>",
     M/binary, "</methodName>",
     Ps/binary, "</methodCall>">>.

coerce_binary(X) when is_atom(X) ->
   coerce_binary(atom_to_list(X));
coerce_binary(X) when is_list(X) ->
   list_to_binary(X);
coerce_binary(X) when is_binary(X) ->
   X.

encode_params(L) ->
   encode_params(L, <<>>).

encode_params([], Acc) ->
   <<"<params>", Acc/binary, "</params>">>;
encode_params([P|R], Acc) ->
   encode_params(R, <<Acc/binary, "<param>", (encode(P))/binary, "</param>">>).

encode(nil) ->
   value(<<"<nil/>">>);
encode(none) ->
   value(<<"<nil/>">>);
encode(I) when is_integer(I) ->
   value(<<"<int>", (integer_to_binary(I))/binary, "</int>">>);
encode(true) ->
   value(<<"<boolean>1</boolean>">>);
encode(false) ->
   value(<<"<boolean>0</boolean>">>);
encode(B) when is_binary(B) ->
   value(<<"<string>", (escape(B))/binary, "</string>">>);
encode(F) when is_float(F) ->
   value(<<"<double>", (float_to_binary(F))/binary, "</double>">>);
encode({{_,_,_},{_,_,_}}=DateTime) ->
   encode({datetime, iso8601:format(DateTime)});
encode({datetime, DateTime}) ->
   value(<<"<dateTime.iso8601>", DateTime/binary, "</dateTime.iso8601>">>);
encode({base64, B}) when is_binary(B) ->
   value(<<"<base64>", B/binary, "</base64>">>);
encode(L) when is_list(L) ->
   value(
      <<"<array><data>",
      (binary:list_to_bin([ encode(E) || E <- L]))/binary,
      "</data></array>">>);
encode({struct, L}) when is_list(L) ->
   value(
      <<"<struct>",
      (binary:list_to_bin([ <<"<member><name>",
                             (escape(coerce_binary(N)))/binary, "</name>",
                             (encode(V))/binary, "</member>">>
                          || {N, V} <- L ]))/binary,
      "</struct>">>).

escape(B) ->
   B1 = binary:replace(B, <<"&">>, <<"&amp;">>, [global]),
   binary:replace(B1, <<"<">>, <<"&lt;">>, [global]).

unescape(B) ->
   B1 = binary:replace(B, <<"&lt;">>, <<"<">>, [global]),
   binary:replace(B1, <<"&amp;">>, <<"&">>, [global]).

value(B) ->
   <<"<value>", B/binary, "</value>">>.

expand_tag(Tag) ->
   BT = coerce_binary(Tag),
   OT = <<"<", BT/binary, ">">>,
   CT = <<"</", BT/binary, ">">>,
   {OT, CT}.

get_tags(Bin, Tag) ->
   {OT, CT} = expand_tag(Tag),
   get_tags(Bin, OT, CT, 0, []).

get_tags(Bin, OT, CT, Offset, Acc) ->
   case get_tag(Bin, OT, CT, Offset) of
      undefined ->
         lists:reverse(Acc);
      {Content, NewOffset} ->
         get_tags(Bin, OT, CT, NewOffset, [binstrip(Content)|Acc])
   end.

get_tag(Bin, OT, CT, Offset) ->
   case first_of(Bin, OT, CT, Offset) of
      {OT, NewOffset} ->
         StartOffset = NewOffset+byte_size(OT),
         case parse_tag(Bin, OT, CT, StartOffset, 1) of
            undefined -> undefined;
            EndOffset ->
               {binary:part(Bin, StartOffset,
                            EndOffset-StartOffset-byte_size(CT)),
                EndOffset}
         end;
      _ -> undefined
   end.

parse_tag(_, _, _, Offset, 0) ->
   Offset;
parse_tag(Bin, OT, CT, Offset, NestCount) ->
   case first_of(Bin, OT, CT, Offset) of
      {OT, OTOffset} ->
         parse_tag(Bin, OT, CT, OTOffset+byte_size(OT), NestCount+1);
      {CT, CTOffset} ->
         parse_tag(Bin, OT, CT, CTOffset+byte_size(CT), NestCount-1);
      _ -> undefined
   end.

first_of(Bin, OT, CT, Offset) ->
   NextOT = next(Bin, OT, Offset),
   NextCT = next(Bin, CT, Offset),
   case {NextOT, NextCT} of
      {undefined, undefined} ->
         undefined;
      {OTOffset, CTOffset} when OTOffset < CTOffset ->
         {OT, OTOffset};
      {_, CTOffset} ->
         {CT, CTOffset}
   end.

next(Bin, Pattern, Offset) ->
   case binary:match(Bin, Pattern,
                     [{scope, {Offset, byte_size(Bin)-Offset}}]) of
      nomatch -> undefined;
      {Start,_} ->
         Start
   end.

decode(<<"<value>", _/binary>>=V) ->
   [BV] = get_tags(V, value),
   decode(binstrip(BV));
decode(<<"<nil/>">>) ->
   none;
decode(<<"<int>", _/binary>>=I) ->
   [BI] = get_tags(I, int),
   binary_to_integer(binstrip(BI));
decode(<<"<i4>", _/binary>>=I) ->
   [BI] = get_tags(I, i4),
   binary_to_integer(binstrip(BI));
decode(<<"<double>", _/binary>>=F) ->
   [BF] = get_tags(F, double),
   binary_to_float(binstrip(BF));
decode(<<"<boolean>", _/binary>>=B) ->
   [BB] = get_tags(B, boolean),
   case binstrip(BB) of
      <<"1">> -> true;
      _ -> false
   end;
decode(<<"<string>", _/binary>>=S) ->
   [BS] = get_tags(S, string),
   unescape(BS);
decode(<<"<dateTime.iso8601>", _/binary>>=DateTime) ->
   [BDT] = get_tags(DateTime, "dateTime.iso8601"),
   iso8601:parse(BDT);
decode(<<"<base64>", _/binary>>=B64) ->
   [BB64] = get_tags(B64, base64),
   {base64, BB64};
decode(<<"<struct>", _/binary>>=S) ->
   [BS] = get_tags(S, struct),
   [ parse_member(M) || M <- get_tags(BS, member) ];
decode(<<"<array>", _/binary>>=A) ->
   [BA] = get_tags(A, array),
   [Data] = get_tags(BA, data),
   [ decode(V) || V <- get_tags(Data, value) ];
decode(String) ->
   String.

parse_member(M) ->
   [Name|_] = get_tags(M, name),
   [Value] = get_tags(M, value),
   {binstrip(Name), decode(binstrip(Value))}.

binstrip(Bin) ->
   re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).

%% Tests
-ifdef(EUNIT).
encode_decode_test() ->
   ?assertEqual(ok, start()),
   Int = encode(12),
   ?assertEqual(Int, <<"<value><int>12</int></value>">>),
   ?assertEqual(12, decode(Int)),
   Double = encode(1.2),
   ?assertMatch(<<"<value><double>", _/binary>>, Double),
   ?assertEqual(1.2, decode(Double)),
   True = encode(true),
   ?assertEqual(True, <<"<value><boolean>1</boolean></value>">>),
   ?assertEqual(true, decode(True)),
   False = encode(false),
   ?assertEqual(False, <<"<value><boolean>0</boolean></value>">>),
   ?assertEqual(false, decode(False)),
   Nil = encode(none),
   Nil2 = encode(nil),
   ?assertEqual(Nil, Nil2),
   ?assertEqual(Nil, <<"<value><nil/></value>">>),
   ?assertEqual(none, decode(Nil)),
   String = encode(<<"test<&">>),
   ?assertEqual(String, <<"<value><string>test&lt;&amp;</string></value>">>),
   ?assertEqual(<<"test<&">>, decode(String)),
   Array = encode([nil, 2, <<"three">>]),
   ?assertEqual(Array, <<"<value><array><data><value><nil/></value><value><int>2</int></value><value><string>three</string></value></data></array></value>">>),
   ?assertEqual([none, 2, <<"three">>], decode(Array)),
   Struct = encode({struct, [{<<"a">>, 1}, {<<"b">>, 2}]}),
   io:format("~p", [Struct]),
   ?assertEqual(Struct, <<"<value><struct><member><name>a</name><value><int>1</int></value></member><member><name>b</name><value><int>2</int></value></member></struct></value>">>),
   ?assertEqual([{<<"a">>, 1}, {<<"b">>, 2}], decode(Struct)),
   Base64 = encode({base64, <<"dGVzdA==">>}),
   ?assertEqual(Base64, <<"<value><base64>dGVzdA==</base64></value>">>),
   ?assertEqual({base64, <<"dGVzdA==">>}, decode(Base64)),
   DateTime = encode({{2013,9,25},{14,57,29}}),
   ?assertEqual(DateTime, <<"<value><dateTime.iso8601>2013-09-25T14:57:29Z</dateTime.iso8601></value>">>),
   ?assertEqual({{2013,9,25},{14,57,29}}, decode(DateTime)),
   ?assertEqual(<<"test">>, decode(<<"test">>)),
   ?assertEqual(12, decode(<<"<value><i4>12</i4></value>">>)),
   ?assertEqual(xmlrequest('test.method', [1]), <<"<?xml version=\"1.0\"?><methodCall><methodName>test.method</methodName><params><param><value><int>1</int></value></param></params></methodCall>">>),
   ?assertEqual(ok, application:stop(xmlrpcc)).
-endif.
