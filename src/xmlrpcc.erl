-module(xmlrpcc).

-export([start/0, call/3, call/4]).
% debug
-export([xmlrequest/2, encode_params/1, encode_params/2, encode/1]).
-export([get_tags/2, get_tag/2 ,parse_tag/5, first_of/4, next/3]).

start() ->
    ok = hackney:start().


call(Url, Method, Args) ->
    call(Url, Method, Args, []).

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

encode(I) when is_integer(I) ->
   value(<<"<int>", (integer_to_binary(I))/binary, "</int>">>);
encode(true) ->
   value(<<"<boolean>1</boolean>">>);
encode(false) ->
   value(<<"<boolean>0</boolean>>">>);
encode(B) when is_binary(B) ->
   value(<<"<string>", (escape(B))/binary, "</string>">>);
encode(F) when is_float(F) ->
   value(<<"<double>", (float_to_binary(F))/binary, "</double">>);
encode({{_,_,_},{_,_,_}}=DateTime) ->
   encode({datetime, iso8601:format(DateTime)});
encode({datetime, DateTime}) ->
   value(<<"<dateTime.iso8601>", DateTime/binary, "</dateTime.iso8601>">>);
encode({base64, B}) when is_binary(B) ->
   value(<<"<base64>", B/binary, "</base64>">>);
encode(L) when is_list(L) ->
   <<"<array><data>",
     (binary:list_to_bin([ encode(E) || E <- L]))/binary,
     "</data></array>">>;
encode({struct, L}) when is_list(L) ->
   <<"<struct>",
     (binary:list_to_bin([ <<"<member><name>",
                             (escape(coerce_binary(N)))/binary, "</name>",
                             (encode(V))/binary, "</member">>
                          || {N, V} <- L ]))/binary,
     "</struct>">>.

escape(B) ->
   B1 = binary:replace(B, <<"<">>, <<"&lt;">>, [global]),
   binary:replace(B1, <<"&">>, <<"&amp;">>, [global]).

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

get_tag(Bin, Tag) ->
   {OT, CT} = expand_tag(Tag),
   get_tag(Bin, OT, CT, 0).
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
decode(<<"<nil>">>) ->
   none;
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
   BS;
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
   [Name] = get_tags(M, name),
   [Value] = get_tags(M, value),
   {binstrip(Name), decode(binstrip(Value))}.

binstrip(Bin) ->
   re:replace(Bin, "^\\s+|\\s+$", "", [{return, binary}, global]).
