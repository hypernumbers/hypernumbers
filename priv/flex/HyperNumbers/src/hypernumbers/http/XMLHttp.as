package hypernumbers.http
{
    import mx.rpc.http.HTTPService;

    public class XMLHttp
    {
        public static function post(url:String,data:String, callback:Function):void
        {
            var tmp:HTTPService = new HTTPService();
            var variables:Object = new XML(data);
            tmp.method = "POST";
            tmp.url = url;
            tmp.contentType = "application/xml"
            tmp.resultFormat = "e4x";
            tmp.addEventListener("result", callback);
            tmp.send(variables);
        }

        public static function get(url:String, callback:Function):void
        {
            var tmp:HTTPService = new HTTPService();
            tmp.method = "GET";
            tmp.url = url;
            tmp.resultFormat = "e4x";
            tmp.addEventListener("result", callback);
            tmp.send();
        }
    }
}