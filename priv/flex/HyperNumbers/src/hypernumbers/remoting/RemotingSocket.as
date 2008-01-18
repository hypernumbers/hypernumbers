package hypernumbers.remoting
{
    import flash.events.*;
    import flash.net.Socket;
    import flash.system.Security;
    import flash.utils.ByteArray;

    public class RemotingSocket extends Socket
    {
        private const CTRLJ:int = 10;
        private var _queue:Array = new Array();
        private var returnMsg:String = "";

        public function RemotingSocket(server:String,port:int) 
        {
            this.addEventListener(ProgressEvent.SOCKET_DATA, dataHandler);
            this.addEventListener(Event.CONNECT, connectHandler);

            try {
                this.connect(server, port);
            }catch (error:Error) {
                this.close();
            }
        }

        /**
         * Helper function to write to a socket
         */
        public function write(ba:String):void 
        {
            if(this.connected)
            {
                this.writeUTFBytes(ba);
                this.flush();
            }
            else
            {
                _queue.push(ba);
            }
        }

        /**
         * When socket connects, send out any buffered messages
         */
        private function connectHandler(event:Event):void 
        {
            if (connected) 
            {
                var count:int = _queue.length;
                for (var i:int = 0; i < count; i++)
                {
                    var tmp:String = _queue.pop();
                    this.writeUTFBytes(tmp);
                    this.flush();
                }
            }
        }

        /**
         * Received data, create a new event to send it on
         */
        private function dataHandler(event:ProgressEvent):void 
        {
            var n:int = this.bytesAvailable;

            while (--n >= 0) 
            {
                var b:int = this.readUnsignedByte();

                if (b == CTRLJ)
                {
                    var remotingEvent:RemotingEvent = new RemotingEvent(this.returnMsg);
                    this.dispatchEvent(remotingEvent);
                    returnMsg="";
                }
                else if (b != CTRLJ)
                {
                    returnMsg += String.fromCharCode(b);
                }
            }
        }
    }
}