require 'fileutils'
require 'rubygems'
require 'hpricot'
require 'png'

# Define the make_png function

NT_TO_B26_MAP = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "AA", "AB", "AC", "AD", "AE", "AF", "AG", "AH", "AI", "AJ", "AK", "AL", "AM", "AN", "AO", "AP", "AQ", "AR", "AS", "AT", "AU", "AV", "AW", "AX", "AY", "AZ", "BA", "BB", "BC", "BD", "BE", "BF", "BG", "BH", "BI", "BJ", "BK", "BL", "BM", "BN", "BO", "BP", "BQ", "BR", "BS", "BT", "BU", "BV", "BW", "BX", "BY", "BZ", "CA", "CB", "CC", "CD", "CE", "CF", "CG", "CH", "CI", "CJ", "CK", "CL", "CM", "CN", "CO", "CP", "CQ", "CR", "CS", "CT", "CU", "CV", "CW", "CX", "CY", "CZ", "DA", "DB", "DC", "DD", "DE", "DF", "DG", "DH", "DI", "DJ", "DK", "DL", "DM", "DN", "DO", "DP", "DQ", "DR", "DS", "DT", "DU", "DV", "DW", "DX", "DY", "DZ", "EA", "EB", "EC", "ED", "EE", "EF", "EG", "EH", "EI", "EJ", "EK", "EL", "EM", "EN", "EO", "EP", "EQ", "ER", "ES", "ET", "EU", "EV", "EW", "EX", "EY", "EZ", "FA", "FB", "FC", "FD", "FE", "FF", "FG", "FH", "FI", "FJ", "FK", "FL", "FM", "FN", "FO", "FP", "FQ", "FR", "FS", "FT", "FU", "FV", "FW", "FX", "FY", "FZ", "GA", "GB", "GC", "GD", "GE", "GF", "GG", "GH", "GI", "GJ", "GK", "GL", "GM", "GN", "GO", "GP", "GQ", "GR", "GS", "GT", "GU", "GV", "GW", "GX", "GY", "GZ", "HA", "HB", "HC", "HD", "HE", "HF", "HG", "HH", "HI", "HJ", "HK", "HL", "HM", "HN", "HO", "HP", "HQ", "HR", "HS", "HT", "HU", "HV", "HW", "HX", "HY", "HZ", "IA", "IB", "IC", "ID", "IE", "IF", "IG", "IH", "II", "IJ", "IK", "IL", "IM", "IN", "IO", "IP", "IQ", "IR", "IS", "IT", "IU", "IV"]

$htmlpage=""
$yzoom=4
$xzoom=$yzoom*8

# Column name -> index, e.g. AZ -> 52.
def b26toi(str)
  str.downcase.unpack('C*').map { |x| # make a list of ASCII codes
    x - 96 # convert to list of numbers in range [1..26]
  }.inject { |acc, x|
    x + acc * 26
  }
end

def is_higher(a,b)
	if a > b
	  return a
	else
	  return b
	end
end

def colour_block(canvas,x,y,colour)
	xstart=x*$xzoom
	ystart=y*$yzoom
	xend=xstart+$xzoom
	yend=ystart+$yzoom
	# puts "x #{x} y#{y} xstart #{xstart} ystart #{ystart} xend #{xend} yend #{yend}"
	while xstart < xend-1
		while ystart < yend-1
			canvas[xstart+1,ystart+1]=colour
		  	ystart += 1
		end
		ystart=y*$yzoom
		xstart += 1
	end
end		 

def make_png(file)
	@data = []
	$limits={}
	$canvas={}
	$offsets={}
	newfile="../../logs/"+file
	# puts newfile
	segments=newfile.split("/")
	# puts segments
	pngnames=segments[4].split(".")
	pngname=pngnames[1]
	puts pngname
	# puts pngname
	doc=open("../../logs/"+file)  {|f| Hpricot(f) }
	table=doc.at("table")
	rows=table.search("tr")
	rows.each do |r|
	 cells=r.search('//td')
	 if (cells.first && cells.first.to_s != "<td></td>")
	 	file=cells[1].inner_text
	 	split=cells[2].inner_text.split("_")
	 	sheet=file+"_"+split[0]
	 	c = b26toi(split[1][/[a-zA-Z]+/])
 		r = split[1][/[0-9]+/].to_i
 		result=cells[5].inner_text
	 	# puts "Result: #{sheet} C#{c} R#{r} #{result}"
	 	@data << [sheet,c,r,result]
	 	if $limits[sheet] != nil
	 	 # puts "running!"
	 	 vals=$limits[sheet]
	 	 chigh=vals[0]
	 	 rhigh=vals[1]
	 	 # puts "#{sheet} X#{c} Y#{r} XU#{chigh} YU#{rhigh}"
	 	 $limits[sheet]=[is_higher(chigh,c),is_higher(rhigh,r)]
	       else
	         # puts "initialising!"
	 	 # puts "#{sheet} X#{c} Y#{r}"
	         $limits[sheet]=[c,r]
	       end	
	 end
	end
	 # puts $limits
	 # puts "starting the dump!"
	 $limits.each do |lim|
	 	# puts "new lim"
	 	xupper=(lim[1][0]+1)*$xzoom+2
	 	yupper=(lim[1][1]+1)*$yzoom+2
	 	# puts "#{lim[0]} xupper is #{xupper} yupper is #{yupper}"
	 	$canvas[lim[0]] = PNG::Canvas.new xupper,yupper,(PNG::Color::Black)
 		# puts $canvas[lim[0]].to_s
 	 end
	 # now colour it
	 @data.each do |line|
	 	# puts "New Line"
	 	sheet=line[0]
	 	x=line[1]
	 	y=line[2]
	 	limits=$limits[sheet]
	 	# puts "limits are #{limits}"
	 	yupper=(limits[1]+1)
	 	# puts "yupper is #{yupper}"
	 	result=line[3]
 	        canvas=$canvas[sheet]
	 	# puts "Writing colours #{sheet} X:#{x} Y:#{y} Yu:#{yupper} R:#{result}"
	 	# y goes down for us - not up...
	 	if result == "Ok"
	 	     colour_block(canvas,x,yupper-y-1,PNG::Color::Green)
	 	elsif result == "FAILED"
	 	     colour_block(canvas,x,yupper-y-1,PNG::Color::Red)
	 	elsif result == "SKIPPED"
	 	     colour_block(canvas,x,yupper-y-1,PNG::Color::Yellow)	 	
		end	 	     
	 end
	 $canvas.each do |png|
		puts png[0].to_s
	 	subfile=png[0]
	 	filename=pngname+"_"+subfile+".png"
	 	path="../../logs/visualisation/"
	 	# puts filename
	 	pngfile = PNG.new png[1]
	 	pngfile.save path+filename
	 	rowdetails="<tr><td>&nbsp;</td></tr><tr><td>&nbsp;</td></tr><tr valign=\"top\"><td>"+pngname+" "+subfile+"</td><td><img src=\""+filename+"\"></td></tr>"
		$htmlpage += rowdetails 
	 end
end

#
# Now start the main
#

# Start making the results page
$htmlpage="<html><head></head><body><h1>Test Failure Visualisation</h1><table>"


# read the index file
doc = open("../../logs/index.html") {|f| Hpricot(f) }
table=doc.at("table")
# puts "The table is #{table}"

rows=table.search("tr")
# puts "rows are #{rows}"

rows.each do |r|
 ahref=r.search('a') #.first[:href]
 if ahref.any?
    # puts "making #{ahref.first.attributes['HREF']}"
 	make_png(ahref.first.attributes['HREF'])
 end
end

# now close off the html file
$htmlpage += "</table></body></html>"
filename="../../logs/visualisation/index.html"
File.open(filename, "w") { |f| f << $htmlpage }
