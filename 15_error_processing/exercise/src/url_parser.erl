-module(url_parser).

-export([parse/1]).
-export([split/1, parse/3, parse/2, parse/4, get_date/1]).

-import_lib("/usr/lib/erlang/lib/crypto-4.3.3/src/crypto.erl").


-spec parse(binary()) -> {ok, map()} | {error, term()}.

parse(URL) when is_binary(URL) ->
    UrlParts = split(URL),
    UrlSize = length(UrlParts),
    parse(UrlParts, UrlSize).


parse([_Protocol, <<>>], 2) ->
    {error, invalid_domain};

parse([_UrlsDomain], 1) ->
    {error, invalid_protocol};

parse([Protocol , Domain | Rest0], UrlSize) when UrlSize > 2->
    Rest1 = lists:filter(fun(Item) ->  Item =/= <<>> end, Rest0),
    parse(Protocol, Domain, Rest1);

parse(UrlParts, _UrlSize)->
    {error, {unexpected, UrlParts}}.


parse(Protocol, Domain, UrlParts) ->
    [MayBeQuery | ReversedRest] = lists:reverse(UrlParts),
    case binary:split(MayBeQuery, <<"=">>) of
        [_] -> parse(Protocol, Domain, <<>>, UrlParts) ;
        [_, _] -> parse(Protocol, Domain, MayBeQuery, lists:reverse(ReversedRest))
end.



parse(Protocol, Domain, Query, UrlParts) ->
    {ok, #{protocol => Protocol,
        domain => Domain,
        path => UrlParts,
        query => Query,
        date => get_date(UrlParts)
    }}.



get_date([Year, Month, Day |_] = UrlParts) when is_list(UrlParts)->
    case {date_part(Year), date_part(Month), date_part(Day)} of
        {{Y, []}, {M, []}, {D, []}} when M >=1 andalso M =< 12 andalso D >=1 andalso D =< 31->
            {Y, M, D};
        _ -> undefined
    end;


get_date(_UrlParts) ->
    undefined.

date_part(DatePart)  ->
    string:to_integer(binary_to_list(DatePart)).


split(Str) when is_binary(Str) ->
    re:split(Str, <<"://|\\?|/">>,[{return, binary}]).
