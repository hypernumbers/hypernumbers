%%%-------------------------------------------------------------------
%%% @author    Gordon Guthrie <gordon@hypernumbers.com>
%%% @copyright (C) 2010 Hypernumbers Ltd
%%% @doc       handles integration with external websites
%%%
%%% @end
%%% Created :  9th April 2010 by Gordon Guthrie 
%%%-------------------------------------------------------------------
-module(hnfuns_integration).

% working functions
-export([
         'facebook.share'/1,
         'twitter.button'/1,
         'google.buynowlist'/1,
         'google.buynow'/1
        ]).

%% not working functions
-export([
         'facebook.like'/1,
         'facebook.likebox'/1,
         'twitter.profile'/1,
         'twitter.search'/1,
         'twitter.list'/1,
         'youtube.channel'/1
        ]).

-include("typechecks.hrl").
-include("muin_records.hrl").
-include("hypernumbers.hrl").

-import(muin_util, [cast/2]).
-import(muin_collect, [ col/2, col/3, col/4 ]).

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

%%
%% Exported functions
%%

%% Hypernumbers Channel Name is hypernumbers
'youtube.channel'([ChannelName]) ->
    C = ?string(ChannelName, ?default_str_rules),
    "<script src=\"http://www.gmodules.com/ig/ifr?url=http://www.google.com/ig/modules/youtube.xml&up_channel=" ++ C ++ "&synd=open&w=320&h=390&title=&border=%23ffffff%7C3px%2C1px+solid+%23999999&output=js\"></script>".

%% Hypernumbers merchant ID is 960226209420618 
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, 1, 0);
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price, Quantity]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, 0);
'google.buynow'([Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg]) ->
    google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg).

google_buy_n1(Merchant, Cur, ItemName, ItemDesc, Price, Quantity, Bg) ->
    M = ?string(Merchant, ?default_str_rules),
    C = ?string(Cur, ?default_str_rules),
    Bg1 = string:to_lower(?string(Bg, ?default_str_rules)),
    case lists:member(string:to_upper(C), ?VALID_ISO_CURRENCIES) of
        false -> ?ERRVAL_VAL;
        true  -> case Bg1 of
                     0 -> google_buy_n2(M, C, ItemName, ItemDesc,
                                            Price, Quantity, "white");
                     1 -> google_buy_n2(M, C, ItemName, ItemDesc,
                                            Price, Quantity, "colored");
                     _ -> ?ERRVAL_VAL
                 end
    end.

google_buy_n2(M, C, ItemName, ItemDesc, Price, Quantity, Bg) ->
    IN = ?string(ItemName, ?default_str_rules),
    ID = ?string(ItemDesc, ?default_str_rules),
    P = ?string(Price, ?default_str_rules),
    Q = ?string(Quantity, ?default_str_rules),
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
    M = ?string(Merchant, ?default_str_rules),
    C = ?string(Currency, ?default_str_rules),
    Bg1 = string:to_lower(?string(Bg, ?default_str_rules)),
    case lists:member(string:to_upper(C), ?VALID_ISO_CURRENCIES) of
        false -> io:format("no currency~n"),
                 ?ERRVAL_VAL;
        true  -> case Bg1  of
                     "0"  -> google_buy_l2(M, C, Type, "white", Rest);
                     "1"  -> google_buy_l2(M, C, Type, "colored", Rest);
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
    exit("fuck up!").

get_google_bits2(Cur, Name, Desc, Price, Quantity, C) ->
    N = ?string(Name, ?default_str_rules),
    D = ?string(Desc, ?default_str_rules),
    P = ?string(Price, ?default_str_rules),
    Q = ?string(Quantity, ?default_str_rules),
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

'facebook.like'([URL]) ->
    fb_like(URL, 0, 0);
'facebook.like'([URL, Layout]) ->
    fb_like(URL, Layout, 0);
'facebook.like'([URL, Layout, Faces]) ->
    fb_like(URL, Layout, Faces).

fb_like(URL, Layout, Faces) ->
    U = ?string(URL, ?default_str_rules),
    L = ?string(Layout, ?default_str_rules),
    F = ?string(Faces, ?default_str_rules),
    case valid(L, F) of
        false -> ?ERRVAL_VAL;
        {L1, F1}  ->
            "<iframe src=\"http://www.facebook.com/plugins/like.php?href="
                ++ U
                ++"&amp;layout="
                ++ L1
                ++"standard&amp;show_faces="
                ++ F1
                ++ "&amp;width=450&amp;action=like&amp;font&amp;colorscheme=light&amp;height=80\" scrolling=\"no\" frameborder=\"0\" style=\"border:none; overflow:hidden; width:450px; height:80px;\" allowTransparency=\"true\"></iframe>"
    end.

valid("0", "0") -> {"standard",     "true"};
valid("1", "0") -> {"button_count", "true"};
valid("0", "1") -> {"standard",     "false"};
valid("1", "1") -> {"button_count", "false"};
valid(_, _) -> false.    
    
%% Hypernumbers Page Id is 336874434418
'facebook.likebox'([PageId]) ->
    P = ?string(PageId, ?default_str_rules),
    Ret = "<iframe src=\"http://www.facebook.com/plugins/likebox.php?id="
        ++ P ++ "&amp;width=292&amp;connections=10&amp;stream=true&amp;header=true\" scrolling=\"no\" frameborder=\"0\" allowTransparency=\"true\" style=\"border:none; overflow:hidden; width:292px; height:200px\"></iframe>",
    io:format("Ret is ~p~n", [Ret]),
    Ret.

'facebook.share'([])       -> fb_share1("box_count", "");
'facebook.share'([0])      -> fb_share1("box_count", "");
'facebook.share'([1])      -> fb_share1("button_count", "");
'facebook.share'([2])      -> fb_share1("button", "");
'facebook.share'([3])      -> fb_share1("icon_count", "");
'facebook.share'([N, URL]) -> Str = ?string(URL, ?default_str_rules),
                            fb_share(N, Str);
'facebook.share'(_Other)   -> ?ERRVAL_VAL.

fb_share(0, URL) -> fb_share1("box_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(1, URL) -> fb_share1("button_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(2, URL) -> fb_share1("button",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(3, URL) -> fb_share1("icon_count",
                                      "share_url=\"" ++ URL ++ "\"");
fb_share(_, _) -> ?ERRVAL_VAL.


fb_share1(Button, URL) ->
    "<a name=\"fb_share\" type=\"" ++ Button ++ "\" "  ++ URL++ " href=\"http://www.facebook.com/sharer.php\">Share</a><script src=\"http://static.ak.fbcdn.net/connect.php/js/FB.Share\" type=\"text/javascript\"></script>".

%% Hypernumbers Twitter UserName is hypernumbers
'twitter.button'([UserName]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b1(0, "a", Str);
'twitter.button'([UserName, Type]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b1(Type, "a", Str);
'twitter.button'([UserName, N, Colour]) ->
    Str = ?string(UserName, ?default_str_rules),
    tw_b(N, Colour, Str).

tw_b(N, 0, Str) -> tw_b1(N, "a", Str);
tw_b(N, 1, Str) -> tw_b1(N, "b", Str);
tw_b(N, 2, Str) -> tw_b1(N, "c", Str);
tw_b(_, _, _)   -> ?ERRVAL_VAL.

tw_b1(0, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/follow_me-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(1, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/follow_bird-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(2, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/twitter-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(3, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_logo-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(4, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_small-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(5, Colour, UserName) -> "<a href=\"http://www.twitter.com/" ++ UserName ++ "\"><img src=\"http://twitter-badges.s3.amazonaws.com/t_mini-" ++ Colour ++ ".png\" alt=\"Follow " ++ UserName ++ " on Twitter\"/></a>";
tw_b1(_, _, _) -> ?ERRVAL_VAL.

'twitter.profile'([UserName]) ->
    U = ?string(UserName, ?default_str_rules),
    "<script src=\"http://widgets.twimg.com/j/2/widget.js\"></script>"
        ++ "<script>"
        ++ "new TWTR.Widget({"
        ++ "version: 2,"
        ++ "type: 'profile',"
        ++ "rpp: 4,"
        ++ "interval: 6000,"
        ++ "width: 250,"
        ++ "height: 300,"
        ++ "theme: {"
        ++ "shell: {"
        ++ "background: '#333333',"
        ++ "color: '#ffffff'"
        ++ "},"
        ++ "tweets: {"
        ++ "background: '#000000',"
        ++ "color: '#ffffff',"
        ++ "links: '#4aed05'"
        ++ "}"
        ++ "},"
        ++ "features: {"
        ++ "scrollbar: false,"
        ++ "loop: false,"
        ++ "live: false,"
        ++ "hashtags: true,"
        ++ "timestamp: true,"
        ++ "avatars: false,"
        ++ "behavior: 'all'"
        ++ "}"
        ++ "}).render().setUser('" ++ U ++ "').start();"
        ++ "</script>".

%% the bug here is that the jquery script that runs the window doesn't find
%% the global object 'TWTR' which presumably is created by the first script
'twitter.search'([]) ->
    %S = ?string(SearchTerm, ?default_str_rules),
    "<script src=\"http://widgets.twimg.com/j/2/widget.js\"></script>"
        ++ "<script>"
        ++ "new TWTR.Widget({"
        ++ "version: 2,"
        ++ "type: 'search',"
        ++ "search: 'hypernumbers',"
        ++ "interval: 6000,"
        ++ "title: 'Excitement is in the air...',"
        ++ "subject: 'OMG!',"
        ++ "width: 250,"
        ++ "height: 300,"
        ++ "theme: {"
        ++ "shell: {"
        ++ "background: '#8ec1da',"
        ++ "color: '#ffffff'"
        ++ "},"
        ++ "tweets: {"
        ++ "background: '#ffffff',"
        ++ "color: '#444444',"
        ++ "links: '#1985b5'"
        ++ "}"
        ++ "},"
        ++ "features: {"
        ++ "scrollbar: false,"
        ++ "loop: true,"
        ++ "live: true,"
        ++ "hashtags: true,"
        ++ "timestamp: true,"
        ++ "avatars: true,"
        ++ "behavior: 'default'"
        ++ "}"
        ++ "}).render().start();"
        ++ "</script>".

'twitter.list'([]) ->
    "<script src=\"http://widgets.twimg.com/j/2/widget.js\"></script>"
        ++ "<script>"
        ++ "new TWTR.Widget({"
        ++ "version: 2,"
        ++ "type: 'list',"
        ++ "rpp: 30,"
        ++ "interval: 6000,"
        ++ "title: 'Everything we do at',"
        ++ "subject: 'the twoffice',"
        ++ "width: 250,"
        ++ "height: 300,"
        ++ "theme: {"
        ++ "shell: {"
        ++ "background: '#ff96e7',"
        ++ "color: '#ffffff'"
        ++ "},"
        ++ "tweets: {"
        ++ "background: '#ffffff',"
        ++ "color: '#444444',"
        ++ "links: '#b740c2'"
        ++ "}"
        ++ "},"
        ++ "features: {"
        ++ "scrollbar: true,"
        ++ "loop: false,"
        ++ "live: true,"
        ++ "hashtags: true,"
        ++ "timestamp: true,"
        ++ "avatars: true,"
        ++ "behavior: 'all'"
        ++ "}"
        ++ "}).render().setList('twitter', 'more-twitter-accounts').start();"
        ++ "</script>".
