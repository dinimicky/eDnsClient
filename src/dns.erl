-module(dns).
-compile(export_all).

-include_lib("kernel/src/inet_dns.hrl").

-define(ALIST, [
                {"icscf1.iv.ims.cbc",[{192,168,1,100}, {192,168,1,101}]},
                {"icscf2.iv.ims.cbc",[{192,168,55,242}, {192,168,55,243}]},
                {"ims.vodafone.pt",[{10,170,1,10}, {10,170,1,11}]}
               ]).

-define(SRVLIST, [{"_sip._udp.cisco4.iv.ims.cbc",[{"icscf2.iv.ims.cbc",5120}]},
                  {"_sip._udp.icscf.iv.ims.cbc", [{"icscf1.iv.ims.cbc", 5060},{"icscf2.iv.ims.cbc", 5060}]},
                  {"_sip._udp.920.iv.ims.cbc", [{"920.iv.ims.cbc", 5061}]}
                 ]).

-define(NAPTRLIST, [{"4.3.2.1.0.2.1.5.2.4.2.8.8.8.0.0.e164.arpa",["!^.*$!sip:8882425120123456@cisco4.iv.ims.cbc!"]},
                    {"5.4.3.2.1.0.2.2.2.4.4.4.e164.arpa",["!^.*$!sip:444222012345@cisco4.iv.ims.cbc!"]},
                    {"0.9.8.7.6.5.e164.arpa",["!^.*$!sip:567890@icscf.iv.ims.cbc!"]},
                    {"5.4.3.2.1.e164.arpa", ["!^.*$!sip:12345@cisco4.iv.ims.cbc!"]}
                   ]).
q(Domain, NS, Type) ->
    Query = inet_dns:encode(
        #dns_rec{
            header = #dns_header{
                id = crypto:rand_uniform(1,16#FFFF),
                opcode = 'query',
                rd = true
            },
            qdlist = [#dns_query{
                domain = Domain,
                type = Type,
                class = in
            }]
        }),
    {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
    gen_udp:send(Socket, NS, 53, Query),
    {ok, {NS, _P, Reply}} = gen_udp:recv(Socket, 65535, 1000),
    case inet_dns:decode(Reply) of
		{ok, #dns_rec{header = Header} = Ans} -> 
			#dns_header{rcode=RC} = Header,
			case RC of 
				0 -> {ok, Ans};
				1 -> {error, format_error};
				3 -> {error, name_error};
				4 -> {error, not_implemented}
			end;
		{error, Reason} -> {error, Reason}
	end.

testok(Type) ->
    case Type of
    a ->     {ok, _} = q("ims.vodafone.pt", {127,0,0,1}, a);
    srv ->   {ok, _} = q("_sip._udp.icscf.iv.ims.cbc", {127,0,0,1}, srv);
    naptr -> {ok,_}=dns:q("5.4.3.2.1.e164.arpa", {127,0,0,1}, naptr)
    end.

testnok(Type) ->
    case Type of
    a ->     {ok, _} = q("ims.vodafone1.pt", {127,0,0,1}, a);
    srv ->   {ok, _} = q("_sip._udp.icscf1.iv.ims.cbc", {127,0,0,1}, srv);
    naptr -> {ok,_}=dns:q("6.5.4.3.2.1.e164.arpa", {127,0,0,1}, naptr)
    end.


