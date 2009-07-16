-module(render).

-export([run/0]).

run() ->
    Page = "http://example.com/some/page/",
    {XML, _Residual} = xmerl_scan:file("./xml/example1.xml"),
    XMLTerms = xmerl_lib:simplify_element(XML),
    {Bindings, HTML} = break_out(strip_whitespace(XMLTerms)),
    Static  = make(HTML, Bindings, Page, static),
    Dynamic = make(HTML, Bindings, Page, dynamic),
    write("./html/static.html", Static),
    write("./html/dynamic.html", Dynamic).

make([HTML], Bds, Pg, Type) when Type == static ->
    make_top("") ++ lists:flatten(make2(HTML, Bds, Pg, Type)) 
        ++ make_bottom(Type);
make([HTML], Bds, Pg, Type) when Type == dynamic -> 
    Insert = convert(Bds),
    make_top(Insert) ++ lists:flatten(make2(HTML, Bds, Pg, Type)) 
        ++ make_bottom(Type).

make2([], _, _, _) -> "";
make2({Tag, Attrs, List}, Bds, Pg, Type) ->
    {Start, End} = make_element(Tag, Attrs, Bds, Pg, Type),
    Start ++ [make2(X, Bds, Pg, Type) || X <- List] ++ End.

make_element(Tag, Attrs, Bds, Pg, Type) when Type == static ->
    {Bits, Val} = case lists:keyfind(id, 1, Attrs) of
                      false  -> B = get_bit(Attrs, class),
                                V = "",
                                {B, V};
                      {_, I} -> B = " id=\"" ++ I ++ "\"" 
                                    ++ get_bit(Attrs, class),
                                V = case lists:keyfind(I, 1, Bds) of
                                        false      -> "";
                                        {_, S, _R} -> get_lookup(Pg, S)
                                    end,
                                {B,V}
                  end,
    Tag2 = atom_to_list(Tag),
    Bra = "<" ++ Tag2 ++ Bits ++ ">",
    Ket = "</" ++ Tag2 ++ ">\n",
    {Bra ++ Val, Ket};
make_element(Tag, Attrs, Bds, Pg, Type) when Type == dynamic ->
    {Bits, Val} = case lists:keyfind(id, 1, Attrs) of
                      false  -> B = get_bit(Attrs, class),
                                V = "",
                                {B, V};
                      {_, I} -> B = " id=\"" ++ I ++ "\"" 
                                    ++ get_bit(Attrs, class),
                                V = case lists:keyfind(I, 1, Bds) of
                                        false      -> "";
                                        {_, S, R} -> get_value(R, Pg, S)
                                    end,
                                {B,V}
                  end,
    Tag2 = atom_to_list(Tag),
    Bra = "<" ++ Tag2 ++ Bits ++ ">",
    Ket = "</" ++ Tag2 ++ ">\n",
    {Bra ++ Val , Ket}.

get_bit(Attrs, Key) ->
    case lists:keyfind(Key, 1, Attrs) of
        false  -> "";
        {_, I} -> " " ++ atom_to_list(Key) ++"=\"" ++ I ++ "\""
    end.

get_value("static", Pg, Src)             -> get_lookup(Pg, Src);
get_value(R, _Pg, _Src) 
  when R == "dynamic" orelse R == "both" -> "".

get_lookup(Pg, Source) ->
    "look up the source " ++ Pg ++ Source ++ " statically ".

make_top(Bindings) -> 
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">\n" 
        ++ "<html>\n<head>\n"
        ++ "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=iso-8859-15\">\n"
        ++ "<title>hypernumbers Page</title>\n"
        ++ Bindings ++ "\n"
        ++ "</head>\n".

make_bottom(Type) -> "<!-- Some javascript libraries -->\n"
                         ++ "<script src = \"hn.libs." ++ atom_to_list(Type)
                         ++ ".js\"></script>\n"
                     ++ "</html>".

break_out({renderer, [], [{bindings, [], Bindings} | HTML]}) ->
    {make_bindings(Bindings), HTML}.

make_bindings(List) -> make_bindings(List, []).

make_bindings([], Acc) -> Acc;
make_bindings([{binding, List, []}| T], Acc) ->
    %% don't know the order of the attributes in the binding
    {_, I} = lists:keyfind(identity,   1, List),
    {_, S} = lists:keyfind(source,     1, List),
    {_, R} = lists:keyfind(rendertype, 1, List),
    make_bindings(T, [{I, S, R} | Acc]).
    
strip_whitespace({El, Attr, Children}) ->
  NChild = lists:filter(fun(X) ->
    if 
      is_list(X) -> case re:run(X, "^[\t, \n, " "]*$", []) of
                        {match, _} -> false;
                        nomatch    -> true
                    end;
      true       -> true
    end
  end, Children),
  Ch = lists:map(fun(X) -> strip_whitespace(X) end, NChild),
  {El, Attr, Ch}.

convert(Bindings) -> convert2(Bindings, []).

convert2([], Acc) -> "<!-- A Javascript Object for the libraries to use to bind -->\n"
                         ++ "<script>\nvar Bindings = {\n" 
                         ++ lists:flatten(lists:reverse(Acc)) 
                         ++ "};\n</script>";
convert2([{K, V, _} | T], Acc) -> 
    convert2(T, ["\"" ++ K ++ ": " ++ V ++ "\",\n" | Acc]).

write(FileName, HTML) ->
    case file:open(FileName,[write]) of
        {ok,IOF} ->
            io:format(IOF,"~s",[HTML]),
            file:close(IOF);
        {error,Reason} ->
            io:format("could not open file due to ~p.~n",[Reason])
    end.
