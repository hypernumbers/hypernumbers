<div id="sanity">

<link rel="stylesheet" type="text/css" 
href="/templates/ventris/default.css" />

<div id="outer">

<div id="inner">

<!-- Testing the View Builder -->

<form data-hnid='333' class='hn' data-type='form'>

<h1>Tests for Input type</h1>

<h2>Completeness Tests</h2>
<p>Test 1 a</p>
<div data-hnid='1' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B8'></div>
<p>Test 1 b</p>
<div data-hnid='2' data-type='input' class='hn' data-binding-to='/u/gordon/tiny/B8'></div>
<h2>Cell Tests</h2>
<p>Test 2 a - round trip</p>
<div data-hnid='3' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B8'></div>
<p>Test 2 b - in from one, out to another</p>
<div data-hnid='4' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B8' data-binding-to='/u/gordon/tiny/B9'></div>

<h2>Row Refs</h2>
<p>Test 3a</p>
<div data-hnid='5' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/8:9' data-binding-to='/u/gordon/tiny/8:9'></div>

<h2>Col Refs</h2>
<p>Test 4a</p>
<div data-hnid='6' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B:C' data-binding-to='/u/gordon/tiny/B:C'></div>

<h2>Range Refs</h2>
<p>Test 5a</p>
<div data-hnid='7' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3:C6' data-binding-to='/u/gordon/tiny/B3:C6'></div>

<h2>Last Row Refs</h2>
<p>Test 6a</p>
<div data-hnid='8' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/A-8:C-9' data-binding-to='/u/gordon/tiny/A-8:C-9'></div>

<h2>Last Col Refs</h2>
<p>Test 7a</p>
<div data-hnid='9' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B:C' data-binding-to='/u/gordon/tiny/-B2:-C3'></div>

<h2>Last Range Refs</h2>
<p>Test 8a</p>
<div data-hnid='10' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/-B3:-C6' data-binding-to='/u/gordon/tiny/-B3:-C6'></div>
<p>Test 8b</p>
<div data-hnid='11' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B-3:C-6' data-binding-to='/u/gordon/tiny/B-3:C-6'></div>


<h2>Inconsistent From And To</h2>
<p>Test 9a Cell and Range</p>
<div data-hnid='12' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/B3:C6'></div>

<p>Test 9b Cell and Row</p>
<div data-hnid='13' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/3:6'></div>

<p>Test 9c Cell and Column</p>
<div data-hnid='14' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='/u/gordon/tiny/B:C'></div>

<p>Test 9d Non-Congruent Ranges </p>
<div data-hnid='15' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3:C4' data-binding-to='/u/gordon/tiny/B3:D5'></div>

<p>Test 9e Cell and Last-Col (Congruent)</p>
<div data-hnid='16' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B1' data-binding-to='/u/gordon/tiny/-B1:-B2'></div>

<p>Test 9f Range and Last-Col (Incongruent)</p>
<div data-hnid='17' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B1:D1' data-binding-to='/u/gordon/tiny/-B3:-C3'></div>

<p>Test 9f Cell and Last-Row (Congruent)</p>
<div data-hnid='18' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B1' data-binding-to='/u/gordon/tiny/A-2:B-2'></div>

<p>Test 9g Range and Last-Row (Incongruent)</p>
<div data-hnid='19' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B1:B3' data-binding-to='/u/gordon/tiny/A-2:E-5'></div>

<!--
 <h2>Gibberish</h2>
<p>Test 10a Gibberish To</p>
<div data-hnid='12' data-type='input' class='hn' data-binding-from='/u/gordon/tiny/B3' data-binding-to='gibberish*&^%'></div>

<p>Test 10b Gibberish From</p>
<div data-hnid='13' data-type='input' class='hn' data-binding-from='gibberish*&^%' data-binding-to='/u/gordon/tiny/B3'></div>
-->

</form>

</div>
</div>
</div>