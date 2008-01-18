#------------------------------------------------------------------------------
# A nice wrapper for the Hypernumbers API.
# Author: <hasan@hypernumbers.com>
#------------------------------------------------------------------------------

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
      resp.body.to_f
    end

    def post(cell, data, sheet=nil)
      sheet ||= @sheet
      path = sheet + cell.to_s
      resp = @http.post(path, "action=create&value=#{CGI.escape(data.to_s)}")
    end
  end

end
