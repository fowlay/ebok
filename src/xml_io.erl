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
    Prolog = ["<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>"],
    Xml = xmerl:export_simple([TopE], xmerl_xml, [{prolog, Prolog}]),
    [$\n | XmlPretty] = lists:flatten(ct_netconfc:format_data(pretty, Xml)),
    FileName = lists:flatten(io_lib:format(?OUTFILE, [Year])),
    {ok, Stream} = file:open(FileName, [write]),
    io:fwrite(Stream, "~s~n", [XmlPretty]),
    file:close(Stream),
    FileName.

make_element(Name, Number) when is_integer(Number) ->
    #xmlElement{name=Name,
                content=[#xmlText{type=text, value=integer_to_list(Number)}]};

make_element(Name, IoList) ->
    #xmlElement{name=Name, content=[#xmlText{type=text, value=IoList}]}.
