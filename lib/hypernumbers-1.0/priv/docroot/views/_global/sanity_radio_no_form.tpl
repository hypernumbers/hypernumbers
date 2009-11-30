<div id="sanity">

<link rel="stylesheet" type="text/css" 
href="/templates/ventris/default.css" />

<div id="outer">

<div id="inner">

<!-- Testing the View Builder -->

<h1>Tests for Radio type</h1>

<h2>Completeness Tests</h2>
<p>Test 1a</p>
<div data-hnid='1' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B8'></div>
<p>Test 1b</p>
<div data-hnid='2' data-type='radio' class='hn' data-binding-to='/u/gordon/tiny/B8'></div>

<h2>Valid Tests</h2>
<p>Test 2a - cell to cell</p>
<div data-hnid='3' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B9'></div>

<p>Test 2b - range to cell</p>
<div data-hnid='4' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/A8:B8' data-binding-to='/u/gordon/tiny/B9'></div>

<p> Test 2c - column to cell</p>
<div data-hnid='5' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/A:B' data-binding-to='/u/gordon/tiny/C9'></div>

<p>Test 2d - row to cell</p>
<div data-hnid='6' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/8:8' data-binding-to='/u/gordon/tiny/B9'></div>

<h2>Cell Tests</h2>
<p>Test 3a - round trip</p>
<div data-hnid='7' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B8'></div>

<p>Test 3b - in from one, out to another</p>
<div data-hnid='8' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B9'></div>

<h2>Row Refs</h2>
<p>Test 4a</p>
<div data-hnid='9' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/8:9' data-binding-to='/u/gordon/tiny/8:9'></div>

<h2>Col Refs</h2>
<p>Test 5a</p>
<div data-hnid='10' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B:C' data-binding-to='/u/gordon/tiny/B:C'></div>

<h2>Range Refs</h2>
<p>Test 6a</p>
<div data-hnid='11' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3:C6' data-binding-to='/u/gordon/tiny/B3:C6'></div>

<h2>Last Row Refs</h2>
<p>Test 7a</p>
<div data-hnid='12' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/A-8:C-9' data-binding-to='/u/gordon/tiny/A-8:C-9'></div>

<h2>Last Col Refs</h2>
<p>Test 8a</p>
<div data-hnid='13' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B:C' data-binding-to='/u/gordon/tiny/-B2:-C3'></div>

<h2>Last Range Refs</h2>
<p>Test 9a</p>
<div data-hnid='14' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/-B3:-C6' data-binding-to='/u/gordon/tiny/-B3:-C6'></div>

<p>Test 9b</p>
<div data-hnid='15' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B-3:C-6' data-binding-to='/u/gordon/tiny/B-3:C-6'></div>

<h2>Inconsistent From And To</h2>
<p>Test 10a Cell and Range</p>
<div data-hnid='16' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/B3:C6'></div>

<p>Test 10b Cell and Row</p>
<div data-hnid='17' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/3:6'></div>

<p>Test 10c Cell and Column</p>
<div data-hnid='18' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/B:C'></div>

<p>Test 10d Non-Congruent Ranges </p>
<div data-hnid='19' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3:C4' data-binding-to='/u/gordon/tiny/B3:D5'></div>

<p>Test 10e Cell and Last-Col (Congruent)</p>
<div data-hnid='20' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B1' data-binding-to='/u/gordon/tiny/-B1:-B2'></div>

<p>Test 10f Range and Last-Col (Incongruent)</p>
<div data-hnid='21' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B1:D1' data-binding-to='/u/gordon/tiny/-B3:-C3'></div>

<p>Test 10f Cell and Last-Row (Congruent)</p>
<div data-hnid='22' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B1' data-binding-to='/u/gordon/tiny/A-2:B-2'></div>

<p>Test 10g Range and Last-Row (Incongruent)</p>
<div data-hnid='23' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B1:B3' data-binding-to='/u/gordon/tiny/A-2:E-5'></div>

<p>Test 10h - cell to cell</p>
<div data-hnid='3' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B8'></div>

<p>Test 10i - range to cell</p>
<div data-hnid='4' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/A8:B8' data-binding-to='/u/gordon/tiny/B8'></div>

<p> Test 10j - column to cell</p>
<div data-hnid='5' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/A:B' data-binding-to='/u/gordon/tiny/B9'></div>

<p>Test 10k - row to cell</p>
<div data-hnid='6' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/8:8' data-binding-to='/u/gordon/tiny/B8'></div>

<!--
<h2>Gibberish</h2>
<p>Test 11a Gibberish To</p>
<div data-hnid='24' data-type='radio' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='gibberish*&^%'></div>

<p>Test 11b Gibberish From</p>
<div data-hnid='25' data-type='radio' class='hn' data-binding-from='gibberish*&^%' data-binding-to='/u/gordon/tiny/B3'></div>
-->

</div>
</div>
</div>