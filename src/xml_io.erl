%% @author Rabbe.Fogelholm
%% @doc @todo Add description to xml_io.


-module(xml_io).

-include_lib("xmerl/include/xmerl.hrl").


-define(FILE_PATH, "c:/Users/Rabbe.Fogelholm/admin/simple-2021.xml").
-define(ORG_NR, "500112-9336").

%% ====================================================================
%% API functions
%% ====================================================================
-export([scan/0, generate/5]).



%% ====================================================================
%% Internal functions
%% ====================================================================

generate(Year, ForsMomsEjAnnan, MomsUtgHog, MomsIngAvdr, MomsBetala) ->
    OrgNrE = make_element('OrgNr', ?ORG_NR),
    PeriodE = make_element('Period', io_lib:format("~w12", [Year])),
    ForsMomsEjAnnanE = make_element('ForsMomsEjAnnan', ForsMomsEjAnnan),
    MomsUtgHogE = make_element('MomsUtgHog', MomsUtgHog),
    MomsIngAvdrE = make_element('MomsIngAvdr', MomsIngAvdr),
    MomsBetalaE = make_element('MomsBetala', MomsBetala),
    MomsE = #xmlElement{name='Moms',
                        content=[PeriodE,
                                 ForsMomsEjAnnanE,
                                 MomsUtgHogE,
                                 MomsIngAvdrE,
                                 MomsBetalaE
                                ]},
    TopE = #xmlElement{name=eSKDUpload,
                       attributes=[#xmlAttribute{name='Version', value="6.0"}],
                       content=[OrgNrE, MomsE]},
    Prolog = ["<?xml version='1.0' encoding='utf-8' ?>"],
    T = xmerl:export_simple([TopE], xmerl_xml
                           , [{prolog, Prolog}]
                           ),
    say("T: ~s", [T]).
    

scan() ->
    {R, []} = xmerl_scan:file(?FILE_PATH, [{validation, off}]),
    %% say("R: ~p", [R]).
    T = xmerl:export_simple_element(R, xmerl_xml),
    say("T: ~s", [T]).


say(Format, Data) ->
    io:fwrite(Format++"~n", Data).

make_element(Name, Float) when is_float(Float) ->
    #xmlElement{name=Name, content=[#xmlText{type=text, value=io_lib:format("~w", [trunc(Float)])}]};

make_element(Name, IoList) ->
    #xmlElement{name=Name, content=[#xmlText{type=text, value=IoList}]}.


