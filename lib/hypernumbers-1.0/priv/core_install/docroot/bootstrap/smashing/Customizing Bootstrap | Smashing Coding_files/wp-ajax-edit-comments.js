/*WP Ajax Edit Script
--Created by Ronald Huereca
--Created on: 03/28/2007
--Last modified on: 09/17/2009
--Relies on jQuery, wp-ajax-response, thickbox
	Copyright 2007,2008  Ronald Huereca  (email : ron alfy [a t ] g m ail DOT com)

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
var tb_pathToImage = "http://www.smashingmagazine.com/wp-includes/js/thickbox/loadingAnimation.gif";

jQuery(document).ready(function() {
    var $j = jQuery;
    $j.ajaxeditcomments = {
        init: function() {
            $j.extend($j.ajaxeditcomments.vars, {
                timers: {},
                timerObjs: {}
            });
            initialize_links();
        },
        delink: function(obj) {
            _delink($j(obj));
        },
        move: function(obj) {
            _thickbox($j(obj));
        },
        edit: function(obj) {
            _thickbox($j(obj));
        },
        request_deletion: function(obj) {
            _thickbox($j(obj));
        },
        approve: function(obj) {
            _approve($j(obj));
        },
        spam: function(obj) {
            _spam($j(obj));
        },
        moderate: function(obj) {
            _moderate($j(obj));
        },
        delete_comment: function(obj) {
            _delete_comment($j(obj));
        },
        remove_comment: function(id) {
            var li = $j("#" + "comment-" + id);
            if (li.is("li") || li.is("div") ) {
                li.addClass("ajax-delete");
                li.slideUp(1000, function() {
                    li.remove();
                });
            }
        },
        retrieve_element: function(id) {
            return $j("#" + id);
        },
        update_comment: function(id, content) {
            $j("#" + id).html(content);
        },
        update_author: function(id, author, url) {
            if (url == '' || 'http://' == url) {
                if (author == '') {
                    $j("#" + id).html(wpajaxeditcomments.AEC_Anon);
                    return;
                }
                $j("#" + id).html(author);
            } else if (author == '') {
                $j("#" + id).html(wpajaxeditcomments.AEC_Anon);
            } else {
                $j("#" + id).html("<a href='" + url + "'>" + author + "</a>");
            }
        },
        remove_element: function(id) {
            var li = $j(id);
            if (li.is("li") || li.is("div") ) {
                li.addClass("ajax-unapprove");
                li.slideUp(1000, function() {
                    li.remove();
                });
            }
        },
        vars: {}
    };
    var vars = $j.ajaxeditcomments.vars;
    //Initializes the edit links
    function initialize_links() {
        //Leave the style in for Safari
        $j(".edit-comment-admin-links").attr("style", "display: block");
        $j(".edit-comment-user-link").attr("style", "display: block");
        /* For Crappy IE */
        $j(".edit-comment-admin-links").show();
        $j(".edit-comment-user-link").show();
        if (wpajaxeditcomments.AEC_CanScroll == "1") {
            var location = "" + window.location;
            var pattern = /(#[^-]*\-[^&]*)/;
            if (1==2&&pattern.test(location)) {
                location = $j("" + window.location.hash);
                var targetOffset = location.offset().top;
                $j('html,body').animate({
                    scrollTop: targetOffset
                }, 1000);
            }
        }
        get_time_left();
    }
    //Finds an area (if applicable) and displays the time left to comment
    function get_time_left() {
        $j("." + 'ajax-edit-time-left').each(function() {
            data = pre_process($j(this).prev());
            data.data = $j.extend({
                action: 'gettimeleft',
                cid: data.cid,
                pid:data.pid,
                _ajax_nonce: data.nonce
            },'');
            data.action = 'gettimeleft';
            data.success = function(r) {
                var res = wpAjax.parseAjaxResponse(r, data.response,data.element);
                jQuery.each( res.responses, function() {
                    if (this.what == "error" || this.what == "success") {
                        return;
                    }
                    if (this.what == "minutes") {
                        minutes = parseInt(this.data);
                    }
                    if (this.what == "seconds") {
                        seconds = parseInt(this.data);
                    }
                });
                cid = data.cid;
                element = $j("#ajax-edit-time-left-" + data.cid);
                data.timer = $j.extend({
                    minutes: minutes,
                    seconds: seconds,
                    cid: data.cid,
                    element: element
                },'');
                vars.timerObjs[data.cid] = data;
                vars.timers[data.cid] = setTimeout(function() {
                    get_time_left_timer(data.timer)
                }, 1000);
            }
            $j.ajax(data);
            return;
        })
    }
    //Updates the UI with the correct time left to edit
    //Parameters - timer (obj with timer data)
    function get_time_left_timer(timer) {
        clearTimeout(vars.timers[timer.cid]);
        seconds = timer.seconds - 1;
        minutes = timer.minutes;
        element = timer.element;
        //Check to see if the time has run out
        if (minutes <=0 && seconds <= 0) {
            $j("#edit" + timer.cid).unbind();
            element.remove();
            $j("#edit-comment-user-link-" + timer.cid).remove();
            tb_remove(); //for iframe
            clearTimeout(vars.timers[timer.cid]);
            return;
        }
        if (seconds < 0) {
            minutes -= 1;
            seconds = 59;
        }
        //Create timer text
        var text = "";
        if (minutes >= 1) {
            if (minutes >= 2) {
                text = minutes + " " + wpajaxeditcomments.AEC_Minutes;
            } else {
                text = minutes + " " + wpajaxeditcomments.AEC_Minute;
            }
            if (seconds > 0) {
                text += " " + wpajaxeditcomments.AEC_And + " ";
            }
        }
        if (seconds > 0) {
            if (seconds >= 2) {
                text += seconds + " " + wpajaxeditcomments.AEC_Seconds;
            } else {
                text += seconds + " " + wpajaxeditcomments.AEC_Second;
            }
        }
        //Output the timer to the user
        try {
            //This try statement is for the iFrame
            //Iframe code from:  http://xkr.us/articles/dom/iframe-document/
            if (document.getElementById('TB_iframeContent') != 'undefined') {
                var oIframe = document.getElementById('TB_iframeContent');
                var oDoc = (oIframe.contentWindow || oIframe.contentDocument);
                if (oDoc.document) oDoc = oDoc.document;
                $j("#timer" + timer.cid, oDoc).html("&nbsp;(" + text + ")");
            }
        } catch(err) { }
        $j("#ajax-edit-time-left-" + timer.cid).html("&nbsp;(" + text + ")");
        timer.minutes = minutes;
        timer.seconds = seconds;
        vars.timerObjs[timer.cid] = timer;
        vars.timers[timer.cid] = setTimeout(function() {
            get_time_left_timer(timer)
        }, 1000);
    }
    //Returns a data object for ajax calls
    function pre_process(element) {
        var s = {};
        s.element = element.attr("id");
        s.response = 'ajax-response';
        var url = wpAjax.unserialize(element.attr('href'));
        s.nonce = url._wpnonce;
        s.cid = url.c;
        s.pid = url.p;
        s.action = url.action;
        s.type = "POST";
        s.url = wpajaxeditcomments.AEC_PluginUrl + "/php/AjaxEditComments.php";
        s.data = $j.extend(s.data, {
            action: s.action,
            cid: s.cid,
            pid:s.pid,
            _ajax_nonce: s.nonce
        });
        s.global = false;
        s.timeout = 30000;
        return s;
    }
    function _delink(obj) {
        var data = pre_process($j(obj));
        data.success = function(r) {
            if (r == 1) {
                $j(".aec-delink-" + data.cid).html("");
                $j("#edit-author" + data.cid).html($j("#edit-author" + data.cid + " A").html()) //for on a post
                return;
            }
            //Delinking wasn't a success, display error
            alert(r);
        }
        $j.ajax(data);
    }
    function _thickbox(obj) {
        obj = $j(obj);
        var data = pre_process(obj);
        //For the Thickbox
        obj.addClass("thickbox");
        var t = obj.attr("title")|| obj.attr("name") || null;
        var a = obj.attr("href") || obj.attr("alt");
        var g = obj.attr("rel") || false;
        if (!window.tb_show) {
            jQuery.tb_show(t,a,g); //NextGen Compatibility
        } else {
            tb_show(t,a,g);
        }
        obj.blur();
    }
    function _approve(obj) {
        var data = pre_process($j(obj));
        data.success = function(r) {
            if (r == 1) {
                //Yay, comment is approved - Show visual
                var li = $j("#" + "comment-" + data.cid);
                if (li.is("li") || li.is("div") ) {
                    li.addClass("ajax-approve");
                    li.slideUp(1000, function() {
                        li.remove();
                    });
                }
                return;
            }
            //Approval wasn't a success, display error
            alert(r);
        }
        if (confirm(wpajaxeditcomments.AEC_Approve)) {
            $j.ajax(data);
        }
    }
    function _spam(obj) {
        var data = pre_process($j(obj));
        data.success = function(r) {
            if (r == 1) {
                //Yay, comment was marked as spam.  Try to show a visual
                var li = $j("#" + "comment-" + data.cid);
                if (li.is("li") || li.is("div") ) {
                    li.addClass("ajax-delete");
                    li.slideUp(1000, function() {
                        li.remove();
                    });
                }
                return;
            }
            //Spamation wasn't a success, display error
            alert(r);
        }
        if (confirm(wpajaxeditcomments.AEC_Spam)) {
            $j.ajax(data);
        }
    }
    function _moderate(obj) {
        var data = pre_process($j(obj));
        data.success = function(r) {
            if (r == 1) {
                //Yay, comment is unapproved - Show visual
                var li = $j("#" + "comment-" + data.cid);
                if (li.is("li") || li.is("div") ) {
                    li.addClass("ajax-unapprove");
                    li.slideUp(1000, function() {
                        li.remove();
                    });
                }
                return;
            }
            //Unapproval wasn't a success, display error
            alert(r);
        }
        if (confirm(wpajaxeditcomments.AEC_Moderation)) {
            $j.ajax(data);
        }
    }
    function _delete_comment(obj) {
        var data = pre_process($j(obj));
        data.success = function(r) {
            if (r == 1) {
                //Yay, comment was deleted.  Try to show a visual
                var li = $j("#" + "comment-" + data.cid);
                if (li.is("li") || li.is("div") ) {
                    li.addClass("ajax-delete");
                    li.slideUp(1000, function() {
                        li.remove();
                    });
                }
                return;
            }
            //Deletion wasn't a success, display error
            alert(r);
        }
        if (confirm(wpajaxeditcomments.AEC_Delete)) {
            $j.ajax(data);
        }
    }
    if(typeof wpajaxeditcomments != 'undefined')
    {
        $j.ajaxeditcomments.init();
    }
});
//For legacy purposes
var AjaxEditComments = function() {
    return {
        init : function() {
            if(typeof wpajaxeditcomments != 'undefined')
            {
                jQuery.ajaxeditcomments.init();
            }
        }
    };
}();