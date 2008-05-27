# A nice wrapper for the Hypernumbers API.
# <hasan@hypernumbers.com>

require "net/http"
require "cgi"

module Hypernumbers
  
  class Connection
    attr_reader :site
    attr_reader :port
    attr_reader :http
    attr_accessor :sheet

    def initialize(site, port, sheet = nil)
      @site = site
      @port = port
      @sheet ||= "/"
      @http = Net::HTTP.new(site, port)
    end

    def get(cell, sheet=nil)
      sheet ||= @sheet
      resp = @http.get(sheet + cell.to_s)
      resp.body.to_f # TODO: conv_from_get here.
    end

    def post(cell, data, sheet=nil)
      sheet ||= @sheet
      path = sheet + cell.to_s
      # TODO: conv_for_post here.
      req = "<create><formula><![CDATA[#{CGI.escape(data.to_s)}]]></formula></create>"
      resp = @http.post(path, req)
    end
  end

end
