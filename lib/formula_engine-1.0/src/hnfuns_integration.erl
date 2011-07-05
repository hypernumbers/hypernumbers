%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       handles integration with external websites
%%%
%%% @end
%%% Created :  9th April 2010 by Gordon Guthrie
%%%-------------------------------------------------------------------
-module(hnfuns_integration).

%%% TODO - casting rules

% working functions
-export([
         'generic.integration.'/1,
         'facebook.comments'/1,
         'disqus.comments'/1,
         'twitter.button'/1,
         %'google.buynowlist'/1, not compatible with the wizard
         'google.buynow'/1,
         'twitter.tweet'/1,
         'facebook.likebox'/1,
         'facebook.like'/1,
         'twitter.profile'/1,
         'google.map'/1
         %'twitter.list'/1
        ]).

%% not working functions
%-export([
%         'youtube.channel'/1
%         'twitter.search'/1,
%        ]).

-include("spriki.hrl").
-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-define(default_str_rules, [first_array, cast_numbers, cast_bools,
                            cast_blanks, cast_dates ]).

-define(VALID_ISO_CURRENCIES,
        [
         "AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD",
         "AWG", "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF",
         "BMD", "BND", "BOB", "BOV", "BRL", "BSD", "BTN", "BWP",
         "BYR", "BZD", "CAD", "CDF", "CHE", "CHF", "CHW", "CLF",
         "CLP", "CNY", "COP", "COU", "CRC", "CUC", "CUP", "CVE",
         "CZK", "DJF", "DKK", "DOP", "DZD", "EEK", "EGP", "ERN",
         "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GHS", "GIP",
         "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL", "HRK", "HTG",
         "HUF", "IDR", "ILS", "INR", "IQD", "IRR", "ISK", "JMD",
         "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW",
         "KWD", "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL",
         "LTL", "LVL", "LYD", "MAD", "MDL", "MGA", "MKD", "MMK",
         "MNT", "MOP", "MRO", "MUR", "MVR", "MWK", "MXN", "MXV",
         "MYR", "MZN", "NAD", "NGN", "NIO", "NOK", "NPR", "NZD",
         "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN", "PYG",
         "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR",
         "SDG", "SEK", "SGD", "SHP", "SLL", "SOS", "SRD", "STD",
         "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP",
         "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "USN",
         "USS", "UYI", "UYU", "UZS", "VEF", "VND", "VUV", "WST",
         "XAF", "XCD", "XOF", "XPF", "YER", "ZAR", "ZMK", "ZWL"
        ]).

-type html() :: string().
-type zoom() :: 1..20.

%%
%% Exported functions
%%

'google.map'([Long, Lat]) -> 'google.map'([Long, Lat, 10]);
'google.map'([Long, Lat, Zoom]) ->
    muin_collect:col([Long, Lat, Zoom], [eval_funs, fetch, {cast, num}],
        [return_errors, {all, fun is_number/1}],
        fun([NLong, NLat, NZoom]) ->
                'google.map_'(NLong, NLat, NZoom)
        end).

-spec 'google.map_'(number(), number(), zoom()) -> html().
'google.map_'(Lat, Long, Zoom) ->
    Lat2 = muin_util:cast(Lat, str),
    Long2 = muin_util:cast(Long, str),
    HTML = "<iframe width='100%' height='100%' frameborder='0' scrolling='no' "
        ++ "marginheight='0' marginwidth='0' src='http://maps.google.com"
        ++ "/?ie=UTF8&amp;ll=" ++ Lat2 ++ "," ++ Long2
        ++ "&amp;z=" ++ muin_util:cast(Zoom, str) ++ "&amp;output=embed'></iframe>",
    {preview, {"Google Map for Lat: " ++ Lat2 ++ " Long: " ++ Long2,
               8, 16, #incs{}}, HTML}.

%'twitter.search_'(_Term, _Title) ->
%    "Todo".

% for hypernumbers it is:
% * shortname = hypernumbers
% * id = 123132
'disqus.comments'([ShortName]) ->
    [ShortName2]= typechecks:std_strs([ShortName]),
    Id = hnfuns_web:page([]),
    Page = hnfuns_web:site([]) ++ hnfuns_web:page([]),
    HTML = "<div id='disqus_thread'></div>"
        ++ "<a href='http://disqus.com' class='dsq-brlink'>"
        ++ "blog comments powered by <span class='logo-disqus'>Disqus</span></a>",
    Reload = "var disqus_shortname = '" ++ ShortName2 ++ "';"
        ++ "//var disqus_developer = 1;"
        ++ "var disqus_identifier = '" ++ Id ++ "';"
        ++ "var disqus_url = '" ++ Page ++ "';"
        ++ "(function() {"
        ++ "    var dsq = document.createElement('script');"
        ++ "              dsq.type = 'text/javascript'; dsq.async = true;"
        ++ "    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';"
        ++ "    (document.getElementsByTagName('head')[0] ||"
        ++ "         document.getElementsByTagName('body')[0]).appendChild(dsq);"
        ++ "})();",
    Incs = #incs{js_reload = [Reload]},
    {resize, {8, 15, Incs}, HTML}.

'generic.integration.'([W, H, HTML]) ->
    gen_i(W, H, HTML, [], [], [], false);
'generic.integration.'([W, H, HTML, Js, JsRel])->
    gen_i(W, H, HTML, Js, JsRel, [], false);
'generic.integration.'([W, H, HTML, Js, JsRel, CSS]) ->
    gen_i(W, H, HTML, Js, JsRel, CSS, false);
'generic.integration.'([W, H, HTML, Js, JsRel, CSS, ShowPreview]) ->
    [SP1] = typechecks:throw_std_bools([ShowPreview]),
    gen_i(W, H, HTML, Js, JsRel, CSS, SP1).

gen_i(W, H, HTML, Js, JsRel, CSS, ShowPreview) ->
    [Width] = typechecks:throw_std_ints([W]),
    [Height] = typechecks:throw_std_ints([H]),
    [HTML2, Js2, JsRel2, CSS2] = typechecks:std_strs([HTML, Js, JsRel, CSS]),
    Incs = #incs{js = [Js2], js_reload = [JsRel2], css = [CSS2]},
    case ShowPreview of
        false -> {resize, {Width, Height, Incs}, HTML2};
        true  -> {preview, {"Generic Integration", Width, Height, Incs}, HTML2}
    end.

% for hypernumbers it is:
% * id = 196044207084776
'facebook.comments'([Id]) ->
    [Id2] = typechecks:std_ints([Id]),
    Id3 = integer_to_list(Id2),
    HTML = "<div id='fb-root'></div><fb:comments href='"
        ++ hnfuns_web:site([]) ++ "' num_posts='2' width='640'></fb:comments>",
    Js = ["http://connect.facebook.net/en_US/all.js#appId=" ++ Id3 ++ "&amp;"
        ++ "xfbml=1"],
    Reload = ["FB.init('" ++ Id3 ++ "', '/external/xd_receiver.htm');"],
    Incs = #incs{js = Js, js_reload = Reload},
    {preview, {"Facebook Comments", 8, 10, Incs},  HTML}.

'twitter.tweet'([Message]) ->
    tweet2(Message, "Tweet This");
'twitter.tweet'([Message, Link]) ->
    tweet2(Message, Link).

tweet2(Message, Link) ->
    Msg = muin_col_DEPR:collect_string(Message, ?default_str_rules),
    Link = muin_col_DEPR:collect_string(Link, ?default_str_rules),
    "<a href=\"http://twitter.com/home?status=" ++ Msg ++ "\">" ++ Link ++ "</a>".

%% Hypernumbers Channel Name is hypernumbers
'youtube.channel'([ChannelName]) ->
    C = muin_col_DEPR:collect_string(ChannelName, ?default_str_rules),
    "<script src=\"http://www.gmodules.com/ig/ifr?url=http://www.google.com/ig/modules/youtube.xml&up_channel=" ++ C ++ "&synd=open&w=320&h=390&title=&border=%23ffffff%7C3px%2C1px+solid+%23999999&output=js\"></script>".

%% Hypernumbers merchant ID is 960226209420618
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, 1, 0);
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price, Quantity]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, 0);
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg).

google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg) ->
    M = muin_col_DEPR:collect_string(Merchant, ?default_str_rules),
    C = muin_col_DEPR:collect_string(Cur, ?default_str_rules),
    Bg1 = string:to_lower(muin_col_DEPR:collect_string(Bg, ?default_str_rules)),
    case lists:member(string:to_upper(C), ?VALID_ISO_CURRENCIES) of
        false -> ?ERRVAL_VAL;
        true  -> case Bg1 of
                     "0" -> google_buy_n2(M, C, ItemName, ItemDesc,
                                          Price, Quantity, "white");
                     "1" -> google_buy_n2(M, C, ItemName, ItemDesc,
                                          Price, Quantity, "trans");
                     _ -> ?ERRVAL_VAL
                 end
    end.

google_buy_n2(M, C, ItemName, ItemDesc, Price, Quantity, Bg) ->
    IN = muin_col_DEPR:collect_string(ItemName, ?default_str_rules),
    ID = muin_col_DEPR:collect_string(ItemDesc, ?default_str_rules),
    P = muin_col_DEPR:collect_string(Price, ?default_str_rules),
    Q = muin_col_DEPR:collect_string(Quantity, ?default_str_rules),
    "<form action=\"https://checkout.google.com/api/checkout/v2/checkoutForm/Merchant/"
        ++ M ++ "\" id=\"BB_BuyButtonForm\" method=\"post\" name=\"BB_BuyButtonForm\" target=\"_top\">"
        ++ "<input name=\"item_name_1\" type=\"hidden\" value=\"" ++ IN ++ "\"/>"
        ++ "<input name=\"item_description_1\" type=\"hidden\" value=\"" ++ ID ++ "\"/>"
        ++ "<input name=\"item_quantity_1\" type=\"hidden\" value=\"" ++ Q ++ "\"/>"
        ++ "<input name=\"item_price_1\" type=\"hidden\" value=\"" ++ P  ++ "\"/>"
        ++ "<input name=\"item_currency_1\" type=\"hidden\" value=\"" ++ C ++ "\"/>"
        ++ "<input name=\"_charset_\" type=\"hidden\" value=\"utf-8\"/>"
        ++ "<input alt=\"\" src=\"https://checkout.google.com/buttons/buy.gif?merchant_id=" ++ M ++ "&amp;w=117&amp;h=48&amp;style=" ++ Bg ++ "&amp;variant=text&amp;loc=en_US\" type=\"image\"/>"
        ++"</form>".

%% Hypernumbers Merchant ID is 960226209420618
'google.buynowlist'([Merchant, Currency, Type, Bg | Rest]) ->
    M = muin_col_DEPR:collect_string(Merchant, ?default_str_rules),
    C = muin_col_DEPR:collect_string(Currency, ?default_str_rules),
    Bg1 = string:to_lower(muin_col_DEPR:collect_string(Bg, ?default_str_rules)),
    case lists:member(string:to_upper(C), ?VALID_ISO_CURRENCIES) of
        false -> ?ERRVAL_VAL;
        true  -> case Bg1  of
                     "0"  -> google_buy_l2(M, C, Type, "white", Rest);
                     "1"  -> google_buy_l2(M, C, Type, "trans", Rest);
                     _  -> ?ERRVAL_VAL
                 end
    end.

google_buy_l2(M, C, Type, Bg, Rest) ->
    {Selections, Input} = get_google_bits(Type, C, Rest, [], [], 0),
    "<form action=\"https://checkout.google.com/api/checkout/v2/checkoutForm/Merchant/" ++ M ++ "\" id=\"BB_BuyButtonForm\" method=\"post\" name=\"BB_BuyButtonForm\" target=\"_top\">"
        ++ "<table cellpadding=\"5\" cellspacing=\"0\" width=\"1%\">"
        ++ "<tr>"
        ++ "<td align=\"right\" width=\"1%\">"
        ++ Selections
        ++ Input
        ++ "</td>"
        ++ "<td align=\"left\" width=\"1%\">"
        ++ "<input alt=\"\" src=\"https://checkout.google.com/buttons/buy.gif?merchant_id=" ++ M ++ "&amp;w=117&amp;h=48&amp;style=" ++ Bg ++ "&amp;variant=text&amp;loc=en_US\" type=\"image\"/>"
        ++ "</td>"
        ++ "</tr>"
        ++ "</table>"
        ++ "</form>".

get_google_bits(_, _Cur, [], Acc1, Acc2, _C) ->
    {get_sel(Acc1), lists:flatten(lists:reverse(Acc2))};
%% option 0 - no quantities
get_google_bits(0, Cur, [Name, Desc, Price | T], Acc1, Acc2, C) ->
    {Sel, Input} = get_google_bits2(Cur, Name, Desc, Price, 1, C),
    get_google_bits(0, Cur, T, [Sel | Acc1], [Input, Acc2], C + 1);
%% option 1 - with quantities
get_google_bits(1, Cur, [Name, Desc, Price, Quant | T], Acc1, Acc2, C) ->
    {Sel, Input} = get_google_bits2(Cur, Name, Desc, Price, Quant, C),
    get_google_bits(1, Cur, T, [Sel | Acc1], [Input, Acc2], C + 1);
get_google_bits(_, _, _, _, _, _) ->
    exit("invalid type of Google Buy Now button").

get_google_bits2(Cur, Name, Desc, Price, Quantity, C) ->
    N = muin_col_DEPR:collect_string(Name, ?default_str_rules),
    D = muin_col_DEPR:collect_string(Desc, ?default_str_rules),
    P = muin_col_DEPR:collect_string(Price, ?default_str_rules),
    Q = muin_col_DEPR:collect_string(Quantity, ?default_str_rules),
    C1 = integer_to_list(C),
    Sel = "<option value=\"" ++ C1 ++ "\">"
        ++ P ++ " - " ++ N ++ "</option>",
    Input = "<input name=\"item_option_name_" ++ C1 ++ "\""
        ++ " type=\"hidden\" value=\"" ++ N ++ "\"/>"
        ++ "<input name=\"item_option_price_" ++ C1 ++ "\""
        ++ " type=\"hidden\" value=\"" ++ P ++ "\"/>"
        ++ "<input name=\"item_option_description_" ++ C1 ++ "\""
        ++ " type=\"hidden\" value=\"" ++ D ++ "\"/>"
        ++ "<input name=\"item_option_quantity_" ++ C1 ++ "\""
        ++ "type=\"hidden\" value=\"" ++ Q ++ "\"/>"
        ++ "<input name=\"item_option_currency_" ++ Cur ++ "\""
        ++ "type=\"hidden\" value=\"" ++ Cur ++ "\"/>",
    {Sel, Input}.

get_sel(List) -> lists:flatten(["<select name=\"item_selection_1\">",
                                lists:reverse(List),
                                "</select>"]).

'facebook.like'([ID]) ->
    URL = hnfuns_web:site([]) ++ hnfuns_web:page([]),
    'facebook.like'([ID, URL, 0]);
'facebook.like'([ID, URL]) ->
    'facebook.like'([ID, URL, 0]);
'facebook.like'([ID, URL, Layout]) ->
    'facebook.like'([ID, URL, Layout, 0]);
'facebook.like'([ID, URL, Layout, Faces]) ->
    fb_like(ID, URL, Layout, Faces).

fb_like(ID, URL, Layout, Faces) ->
    [I] = typechecks:std_ints([ID]),
    U = muin_col_DEPR:collect_string(URL, ?default_str_rules),
    L = muin_col_DEPR:collect_string(Layout, ?default_str_rules),
    F = muin_col_DEPR:collect_string(Faces, ?default_str_rules),
    HTML = case valid(L, F) of
               false -> ?ERRVAL_VAL;
               {L1, F1}  ->
                   "<iframe src='http://www.facebook.com/plugins/like.php?"
                       ++ "app_id=" ++ integer_to_list(I) ++ "&amp;"
                       ++ "href=" ++ U
                       ++"&amp;layout="
                       ++ L1
                       ++"standard&amp;show_faces="
                       ++ F1
                       ++ "&amp;width=152&amp;action=like&amp;font&amp;colorscheme=light&amp;height=80' scrolling='no' frameborder='0' style='border:none; overflow:hidden; height:80px;' allowTransparency='true'></iframe>"
           end,
    {preview, {"Facebook Like Button", 3, 8, #incs{}}, HTML}.

valid("0", "0") -> {"standard",     "true"};
valid("1", "0") -> {"button_count", "true"};
valid("0", "1") -> {"standard",     "false"};
valid("1", "1") -> {"button_count", "false"};
valid(_, _) -> false.

%% Hypernumbers Page Id is 336874434418
'facebook.likebox'([PageId]) ->
    P = muin_col_DEPR:collect_string(PageId, ?default_str_rules),
    HTML = "<iframe src='http://www.facebook.com/plugins/likebox.php?id="
        ++ P ++ "&amp;width=472&amp;connections=10&amp;stream=true&amp;"
        ++ " header=true' scrolling='no' frameborder='0' "
        ++ " allowTransparency='true' style='border:none; "
        ++ "overflow:hidden; width:472px; height:606px'></iframe>",
    {preview, {"Facebook Like of " ++ P, 6, 31, #incs{}}, HTML}.

%% Hypernumbers Twitter UserName is hypernumbers
'twitter.button'([UserName]) ->
    Str = muin_col_DEPR:collect_string(UserName, ?default_str_rules),
    tw_b1(0, "a", Str);
'twitter.button'([UserName, Type]) ->
    Str = muin_col_DEPR:collect_string(UserName, ?default_str_rules),
    tw_b1(Type, "a", Str);
'twitter.button'([UserName, N, Colour]) ->
    Str = muin_col_DEPR:collect_string(UserName, ?default_str_rules),
    tw_b(N, Colour, Str).

tw_b(N, 0, Str) -> tw_b1(N, "a", Str);
tw_b(N, 1, Str) -> tw_b1(N, "b", Str);
tw_b(N, 2, Str) -> tw_b1(N, "c", Str);
tw_b(_, _, _)   -> ?ERRVAL_VAL.

tw_b1(0, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/follow_me-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(1, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/follow_bird-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(2, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/twitter-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(3, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/t_logo-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(4, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/t_small-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(5, Colour, UserName) -> "<a href='http://www.twitter.com/" ++ UserName ++ "'><img src='http://twitter-badges.s3.amazonaws.com/t_mini-" ++ Colour ++ ".png' alt='Follow " ++ UserName ++ " on Twitter'/></a>";
tw_b1(_, _, _) -> ?ERRVAL_VAL.

'twitter.profile'([UserName]) ->
    ID = "hn_twitter_profile_" ++ string:join(get(path), "_")
        ++ hn_util:obj_to_ref({cell, {get(mx), get(my)}}),
    U = muin_col_DEPR:collect_string(UserName, ?default_str_rules),
    Js = "http://widgets.twimg.com/j/2/widget.js",
    Js_reload = "new TWTR.Widget({"
        ++ "id: '" ++ ID ++ "', " % we have added this
        ++ "version: 2, "
        ++ "type: 'profile', "
        ++ "rpp: 4, "
        ++ "interval: 6000, "
        ++ "width: 235, "
        ++ "height: 398, "
        ++ "theme: {"
        ++ "shell: {background: '#444', color: '#eee'},"
        ++ "tweets: {background: '#ddd', color: '#666', links: '#4444ff'}"
        ++ "}, "
        ++ "features: {scrollbar: false, loop: false, "
        ++ "live: false, hashtags: true,"
        ++ "timestamp: true, avatars: false, behavior: 'all'}"
        ++ "}).render().setUser('" ++ U ++ "').start();",
    HTML = "<div id='" ++ ID ++ "'></div>",
    Incs = #incs{js = [Js], js_reload = [Js_reload]},
    {preview, {"Twitter profile for " ++ U, 3, 20, Incs}, HTML}.

'twitter.list'([User, ListId]) ->
    'twitter.list'([User, ListId, User]);
'twitter.list'([User, ListId, Title]) ->
    'twitter.list'([User, ListId, Title, ListId]);
'twitter.list'([User, ListId, Title, SubTitle]) ->
    [U2, L2, T2, SubT2] = typechecks:std_strs([User, ListId, Title, SubTitle]),
    ID = "hn_twitter_list_" ++ string:join(get(path), "_")
        ++ hn_util:obj_to_ref({cell, {get(mx), get(my)}}),
    Js = "http://widgets.twimg.com/j/2/widget.js",
    Js_reload = "new TWTR.Widget({"
        ++ "version: 2, "
        ++ "id: '" ++ ID ++ "', " % we have added this
        ++ "type: 'list', "
        ++ "rpp: 30, "
        ++ "interval: 6000, "
        ++ "title: '" ++ T2 ++ "', "
        ++ "subject: '" ++ SubT2 ++ "', "
        ++ "width: 235, "
        ++ "height: 340, "
        ++ "theme: {"
        ++ "shell: {background: '#444', color: '#eee'},"
        ++ "tweets: {background: '#ddd', color: '#666', links: '#4444ff'}"
        ++ "}, "
        ++ "features: {scrollbar: true, loop: false, live: true, "
        ++ "hashtags: true, timestamp: true, avatars: true, behavior: 'all'}"
        ++ "}).render().setList('" ++ U2 ++"', '" ++ L2 ++ "').start();",
    HTML = "<div id='" ++ ID ++ "'></div>",
    Incs = #incs{js = [Js], js_reload = [Js_reload]},
    {preview, {"Twitter List", 3, 23, Incs}, HTML}.


