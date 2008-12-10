require 'rubygems'
require 'png'

# Define the make_png function

$yzoom=4
$xzoom=$yzoom*8

def is_higher(a,b)
	if a > b
	  return a
	else
	  return b
	end
end

def make_border(canvas,xsize,ysize,colour)
  # puts "in make_border xsize is #{xsize} and ysize is #{ysize}"
  x=0
  while x < xsize
    canvas[x,0]=colour
    canvas[x,ysize-1]=colour
    x=x+1
  end
  y=0
  while y < ysize
    canvas[0,y]=colour
    canvas[xsize-1,y]=colour
    y=y+1
  end
end

def colour_block(canvas,x,y,colour)
	xstart=x*$xzoom
	ystart=y*$yzoom
	xend=xstart+$xzoom
	yend=ystart+$yzoom
	# puts "x #{x} y #{y} xstart #{xstart} ystart #{ystart} xend #{xend} yend #{yend}"
	while xstart < xend-1
		while ystart < yend-1
			canvas[xstart,ystart]=colour
		  	ystart += 1
		end
		ystart=y*$yzoom
		xstart += 1
	end
end		 

def make_png(name,data,bounds,colourzoom)
  # create a new canvas
  # puts "name is #{name}"
  # puts "data is #{data}"
  # puts "bounds is #{bounds}"
  # puts "colourzoom is #{colourzoom}"
  xupper=bounds[1].to_i
  yupper=bounds[0].to_i
  # puts "yupper is #{yupper}"
  # puts "xupper is #{xupper}"
  canvasx=(xupper+1)*$xzoom+2
  canvasy=(yupper+1)*$yzoom+2
  # puts "canvasx is #{canvasx} and canvasy is #{canvasy}"
  canvas = PNG::Canvas.new canvasx, canvasy ,(PNG::Color::White)
  data.each_key do |line|
    if data[line]
      # puts "line is #{line}"
      data[line].each_key do |cell|
        # puts "cell is #{cell}"
        y=line
        x=cell
        val=data[line][cell]
        # puts "x is #{x} and y is #{y} cell is #{val}"
        shade=val*colourzoom+1
        shade=256-shade.to_i
        # puts "val is #{val} shade is #{shade}"
        colour=PNG::Color.new(shade,shade,shade,255)
        colour_block(canvas,x,yupper-y,colour)
      end
    end
  end
  make_border(canvas,canvasx,canvasy,PNG::Color::Black)
  pngfile = PNG.new canvas
  filename="../../logs/recalc/"+name+".png"
  pngfile.save filename
  rowdetails="<tr><td>&nbsp;</td></tr><tr><td>&nbsp;</td></tr><tr valign=\"top\"><td>"+name+"</td><td><img src=\""+filename+"\"></td></tr>"
  $htmlpage += rowdetails 
end

#
# Now start the main
#

# Start making the results page
$htmlpage="<html><head></head><body><h1>Recalc Visualisation</h1><table>"

# Set up some globals

sheets={}
details={}
bounds={}
count={}
# read the index file
f = File.open("../../logs/recalc/recalc_logs.txt","r")
f.each_line do |line|
  v=line.split(" ")
  # puts "sheets #{v[0]} Row #{v[2]} and Col #{v[4]}"
  name=v[0]
  row=v[2].to_i
  col=v[4].to_i
  if sheets[name]
    details=sheets[name]
    # puts "name is #{name} and details are #{details}"
    bound=bounds[name]
    rupper=bound[0]
    cupper=bound[1]
    maxcount=count[2]
    bounds[name]=[is_higher(rupper,row),is_higher(cupper,col)]
  else
    details={}
    count[name]=1
    bounds[name]=[row,col]
  end
  if (details[row])
    if (details[row][col])
      details[row][col] = details[row][col]+1
      # puts "updating     row is   #{row} and col #{col} with #{details[row][col]}"
      # puts "#{details}"
      count[name]=is_higher(count[name],details[row][col])
    else
      details[row][col] = 1        
      # puts "creating col row is   #{row} and col #{col} with #{details[row][col]}"
      # puts "#{details}"
    end
  else
    details[row] = {}
    details[row][col] = 1
    # puts "creating row row is #{row} and col #{col} with #{details[row][col]}"
    # puts "#{details}"
  end
  # puts "count is #{details[row][col]}"
  sheets[name]=details
end
# puts "bounds are #{bounds}"
# puts "count is #{count}"

# now calculate the colour zoom
maxcount=0
count.each do |c|
  maxcount=is_higher(maxcount,c[1])
end
# puts "maxcount is #{maxcount}"
if maxcount > 256
  colourzoom = 1.0/((maxcount/256).to_i+1)
else
  colourzoom = (256.0/maxcount).to_i
end
# puts "colourzoom is #{colourzoom}"

# Start making the results page
$htmlpage="<html><head></head><body><h1>Recalculation Visualisation</h1><table>"

sheets.each_key do |name|
  # puts "name is #{name}"
  name2=name.gsub("/","_")
  # puts "name is #{name} and name2 is #{name2}"
  data=sheets[name]
  # puts "@data is #{data} of type #{data.class}"
  # data.each_key do |idx|
  #  puts "idx is #{idx}"
  #  if data[idx]
  #     data[idx].each_key do |idx2|
  #      puts "    idx2 is #{idx2} with contents #{data[idx][idx2]}"
  #    end
  #   end
  #end
  # puts "bounds are #{bounds[name]}"
  make_png(name2,data,bounds[name],colourzoom)
end
  
# now close off the html file
$htmlpage += "</table></body></html>"
File.open("../../logs/recalc/index.html", "w") { |f| f << $htmlpage }
