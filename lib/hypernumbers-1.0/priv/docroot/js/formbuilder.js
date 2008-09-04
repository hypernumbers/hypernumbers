        function getPath()
	{
            //console.log("in getPath **starts**");
 	    page=document.location.pathname;
	    segments=page.split("/");
            len=segments.length;
            path="";
            for (i=1; i < len-1; i++)
            {
              path=path+"/"+segments[i];
            };
            path=path+"/";
            //console.log(path);
            //console.log("**ends**");
            return path;
        }

        function parseRef(cell)
        {
            return [
                ((cell.match(/[a-z]+/i)[0]).charCodeAt(0)-64),
                 parseInt(cell.match(/[0-9]+/)[0])];
        }
        
        function handleElement(root,params)
        {
            if(params[0] == "select")
            {
                if ((params[2][0] == "/") || (params[2][0] == "."))
                {
                    get_path=params[2];
                }
                else
                {
                    get_path=root+params[2];
                }
                var select = $("<select />").appendTo($(params[1]));   
                $.get(get_path+"?attr",function(data) 
                {
                    $(data).find("ref").each(function()
                    {
                        if($(this).find("rawvalue").size() > 0)
                        {
                            select.append($("<option>"+
                                $(this).find("rawvalue > string").text()+"</option>"));
                        }
                    });
                });
                post_vars.push({type:"select",input:select,destination:params[3]});
            }
            else if(params[0] == "radio")
            {
                $.get(root+params[2]+"?attr",function(data) 
                {
                    var inputs = new Array();
                    $(data).find("ref").each(function()
                    {
                        if($(this).find("rawvalue").size() > 0)
                        {
                            var n = $(this).find("rawvalue > string").text();
                            var el = $("<input type='radio' name="+params[4]+" value='"+n+"' \>");
                            inputs.push(el);
                            $(params[1]).append(el).append($("<span> "+n+"</span>"));
                        }
                    });
                    post_vars.push({type:"radio",input:inputs,destination:params[3]}); 
                });
            }   
            else if(params[0] == "textinput")
            {
                var el = $("<input type=\"text\" />");
                $(params[1]).append($("<label><span>" + params[2]
                    + "</span></label>").append(el));    
                post_vars.push({type:"text",input:el,destination:params[3]});   
            }
            else if(params[0] == "textarea")
            {
                var el = $("<textarea></textarea>");
                $(params[1]).append($("<label><span>" + params[2]
                    + "</span><br /></label>").append(el));
                post_vars.push({type:"textarea",input:el,destination:params[3]});  
            }
            else if(params[0] == "text")
            {
                $(params[1]).text(params[2]);      
            }
        }
    
        function loadForm(root,range)
        {
        //console.log("in loadForm **starts**");
        //console.log(root);
        //console.log("**ends**");
            $.get(root+range+"?attr",function(data)   
            {
                var reflist = new Array();

                $(data).find("ref").each(function()
                
                {
                    if($(this).find("rawvalue").size() > 0)
                    {
                        var ref = parseRef($(this).attr("ref"));                        
                         if(typeof reflist[ref[1]] == "undefined")
                            reflist[ref[1]] = new Array();
                            
                        reflist[ref[1]][ref[0]-1] = $(this);
                    }
                });
                $.each(reflist,function(i)
                {
                    if(this.length>0)
                    {
                        var params = new Array();
                                                    
                        for(i=0; i < this.length; i++) 
                        {
                            params.push(this[i].find("string").text());
                        }
                        handleElement(root,params);
                    }
                });
            });
        }
        
        function loadRange(cfg)
        {
            $.get(cfg+"a1?attr",function(data)
            {
                var range = $(data).find("rawvalue > string").text();
    
                loadForm(cfg,range);
            });
        }
