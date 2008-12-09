require 'rubygems'
require 'png'

# Define the make_png function

$zoom=4

def colour_block(canvas,x,y,colour)
	xstart=x*$zoom
	ystart=y*$zoom
	xend=xstart+$zoom
	yend=ystart+$zoom
	# puts "x #{x} y#{y} xstart #{xstart} ystart #{ystart} xend #{xend} yend #{yend}"
	while xstart < xend
		while ystart < yend
			canvas[xstart,ystart]=colour
		  	ystart += 1
		end
		ystart=y*$zoom
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
	 	xupper=(lim[1][0]+1)*$zoom
	 	yupper=(lim[1][1]+1)*$zoom
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
$htmlpage="<html><head></head><body><h1>Recalc Visualisation</h1><table>"

# Set up some globals

Details= {},
# read the index file
f = File.open("../../logs/recalc/recalc_logs.txt","r")
f.each_line do |line|
  if line.slice(0,4) == "File"