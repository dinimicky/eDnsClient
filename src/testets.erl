-module(testets).

-define(ALIST, [{"icscf1.iv.ims.cbc",[{192,168,1,100}, {192,168,1,101}]},
                {"icscf2.iv.ims.cbc",[{10,170,1,100}, {10,170,1,101}]},
                {"920.iv.ims.cbc",[{10,170,1,10}, {10,170,1,11}]}
               ]).

-define(SRVLIST, [{"icscf.iv.ims.cbc", [{"icscf1.iv.ims.cbc", 5060},{"icscf2.iv.ims.cbc", 5060}]},
                  {"cisco4.iv.ims.cbc", [{"920.iv.ims.cbc", 5061}]}
                 ]).

-define(NAPTRLIST, [{"+12345",["sip:12345@icscf.iv.ims.cbc"]},
                    {"+67890", ["sip:12345@cisco4.iv.ims.cbc"]}
                   ]).


-export([list2ets/2, test/1]).


list2ets(TableId, [H|T]) ->
    io:format("~p~n", [H]),
    Res = ets:insert(TableId, H),
    io:format("~p~n", [Res]),
    list2ets(TableId, T);
list2ets(_TableId, []) ->
    finish.

test(Name) ->
    TableId = ets:new(Name, [set]),
    case Name of
        alist     -> list2ets(TableId, ?ALIST);
        srvlist   -> list2ets(TableId, ?SRVLIST);
        naptrlist -> list2ets(TableId, ?NAPTRLIST)
    end,
    io:format("get value [~p]~n", ets:lookup(TableId, "icscf1.iv.ims.cbc")),
    TableId.

    


