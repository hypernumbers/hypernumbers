%%% Copyright (c) 2010 Hypernumbers Ltd.  All rights reserved.
%%% 
%%% Developed by: Gordon Guthrie <gordon@hypernumbers.com>
%%%               Hypernumbers   <http://hypernumbers.com>
%%% 
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to
%%% deal with the Software without restriction, including without limitation the
%%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%%% sell copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%% 
%%%   1. Redistributions of source code must retain the above copyright notice,
%%%      this list of conditions and the following disclaimers.
%%%   2. Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimers in the
%%%      documentation and/or other materials provided with the distribution.
%%%   3. Neither the names of Hasan Veldstra or Hypernumbers, nor the names of
%%%      its contributors may be used to endorse or promote products derived
%%%      from this Software without specific prior written permission.

%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
%%% CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%%% WITH THE SOFTWARE.
-module(bingo).

-export([make_cards/3,
         make_card/2]).

make_cards(NoOfCards, CardSize, NoOfNumbers) when is_integer(NoOfCards) andalso NoOfCards > 0 ->
    make_cs(NoOfCards, CardSize, NoOfNumbers, []).

make_cs(0, _CardSize, _NoOfNumbers, Acc) -> Acc;
make_cs(N, CardSize, NoOfNumbers, Acc) ->
    NewAcc = make_card(CardSize, NoOfNumbers),
    make_cs(N-1, CardSize, NoOfNumbers, [NewAcc | Acc]).

make_card(CardSize, NoOfNumbers)
  when (is_integer(CardSize) andalso CardSize > 0)
andalso (is_integer(NoOfNumbers) andalso NoOfNumbers > 0)
andalso (CardSize < NoOfNumbers) ->
    make_c1(CardSize, NoOfNumbers, []).

make_c1(0, _NoOfNumbers, Acc) -> lists:sort(Acc);
make_c1(N, NoOfNumbers, Acc) ->
    NewNum = crypto:rand_uniform(1, NoOfNumbers),
    case lists:member(NewNum, Acc) of
        true  -> make_c1(N, NoOfNumbers, Acc);
        false -> make_c1(N-1, NoOfNumbers, [NewNum | Acc])
    end.
             
