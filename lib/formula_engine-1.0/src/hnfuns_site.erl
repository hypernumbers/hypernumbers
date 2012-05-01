%%% @author    Gordon Guthrie
%%% @copyright (C) 2012, Hypernumbers Ltd (Vixo)
%%% @doc       Functions for administering the site
%%%
%%% @end
%%% Created : 21 Apr 2012 by gordon@vixo.com

-module(hnfuns_site).

-include("spriki.hrl").
-include("errvals.hrl").

-export([
         configure_email/1
         ]).

configure_email([FromEmail]) ->
    configure_email([FromEmail, ""]);
configure_email([_FromEmail, _Signature] = Args) ->
    [FromE2, Sig2] = typechecks:std_strs(Args),
    Valid = hn_util:valid_email(FromE2),
    if
        Valid == false -> ?ERR_VAL;
        Valid == true  -> ok
    end,
    Site = get(site),
    Idx = get(idx),
    V = new_db_wu:read_kvD(Site, site_email),
    % got some validation to do
    % a new email has to be validated
    io:format("V is ~p~n", [V]),
    Rec2 = case V of
               [] ->
                   OrigIdx = null,
                   #site_email{idx = Idx, email = FromE2,
                               email_validated = false, signature = Sig2};
               [{kvstore, site_email, Rec}] ->
                   case Rec of
                       #site_email{email = FromE2, idx = OrigIdx} ->
                           Rec#site_email{idx = Idx, signature = Sig2};
                       #site_email{idx = OrigIdx} ->
                           #site_email{idx = Idx, email = FromE2,
                                       email_validated = false,
                                       signature = Sig2}
                   end
           end,
    % there can only be one configure_email panel at one time
    case OrigIdx of
        I when I == Idx orelse I == null ->
            HTML = "<div class='hn_site_admin'>"
                ++ "<div class='hn_site_admin_top'>Email Configuration</div>"
                ++ "<div>" ++ Rec2#site_email.email ++ "<br />"
                ++ "<em>Has the email been validated?</em> "
                ++ atom_to_list(Rec2#site_email.email_validated) ++ "<br />"
                ++ "<em>Signature is:</em> " ++ Rec2#site_email.signature
                ++ "</div>",
            ok = new_db_wu:write_kvD(Site, site_email, Rec2),
            RSz = #resize{width = 4, height = 9},
            #spec_val{val = HTML, resize = RSz, unique = site_email};
        OldI ->
            Uniq = new_db_wu:idx_to_xrefXD(Site, OldI),
            #xrefX{path = P, obj = {cell, _} = O} = Uniq,
            "already set in " ++ hn_util:list_to_path(P) ++ hn_util:obj_to_ref(O)
    end.
