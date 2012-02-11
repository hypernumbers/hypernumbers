%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd
%%% @doc       functions for handling phones
%%%
%%% @end
%%% Created : 11 Feb 2012 by gordon@hypernumbers.com

-module(hnfuns_phone).

-export([
         'phone.out'/1,
         'phone.in'/1
         ]).

-include("spriki.hrl").
-include("errvals.hrl").

'phone.in'([]) ->
    Title = "Phone In",
    Site = get(site),
    Path = get(path),
    X = get(mx),
    Y = get(my),
    URL = hn_util:refX_to_url(#refX{site = Site, path = Path,
                                    obj = {cell, {X, Y}}}),
    Link = "<a href='" ++ URL ++ "?view=phone' target='hnsoftphone'>Soft Phone</a>",
    {preview, {"Phone In", 1, 1, #incs{}}, Link}.

'phone.out'(List) ->
    Title = "Phone Out",
    Js = ["/webcomponents/hn.phone.js",
          "http://static.twilio.com/libs/twiliojs/1.0/twilio.js"],
    Reload = ["HN.Phone.phone_outbound_reload();"],
    CSS = ["/webcomponents/hn.phone.css"],
    Incs = #incs{js = Js, js_reload = Reload, css = CSS},
    PhoneNo = "07776 251669",
    Payload = {struct, [{'twilio_outboundphone', PhoneNo}]},
    HTML = "<button id='hn-outboundphone' value='Phone' " ++
        "data-phoneno='" ++ PhoneNo ++"' disabled>" ++
        "<div class='hn-outboundphone_img'>" ++ "Phone" ++ "</div>"
        "</button>",
    Form = #form{id = {'phone', Title},
                 kind = "phone",
                 attrs = Payload},
    {webcontrol, {Form, {Title, 2, 2, Incs}}, HTML}.


