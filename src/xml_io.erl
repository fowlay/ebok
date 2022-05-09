%% @author Rabbe.Fogelholm
%% @doc @todo Add description to xml_io.


-module(xml_io).

-include_lib("xmerl/include/xmerl.hrl").


-define(OUTFILE, "moms-~w.xml").

%% ====================================================================
%% API functions
%% ====================================================================
-export([generate/6]).



%% ====================================================================
%% Internal functions
%% ====================================================================

generate(Year, OrgNr, ForsMomsEjAnnan, MomsUtgHog, MomsIngAvdr, MomsBetala) ->
    OrgNrE = make_element('OrgNr', OrgNr),
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
    Xml = xmerl:export_simple([TopE], xmerl_xml
                           , [{prolog, Prolog}]
                           ),
    FileName = lists:flatten(io_lib:format(?OUTFILE, [Year])),
    {ok, Stream} = file:open(FileName, [write]),
    io:fwrite(Stream, "~s~n", [Xml]),
    file:close(Stream),
    FileName.

make_element(Name, Float) when is_float(Float) ->
    #xmlElement{name=Name, content=[#xmlText{type=text, value=io_lib:format("~w", [trunc(Float + 0.5)])}]};

make_element(Name, IoList) ->
    #xmlElement{name=Name, content=[#xmlText{type=text, value=IoList}]}.
