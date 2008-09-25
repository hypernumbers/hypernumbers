// Gordon: 07776251669

import com.bt.sdk.capabilities.messaging.oneway.*;

public class ZhopaSms
{
	public static void main(String[] args)
	{
        String smstext = "";
        for(int i = 1; i < args.length; i++) smstext += args[i] + " ";
		MessagingOneWayManager mm = new MessagingOneWayManager();
		Message msg = mm.sendMessage("tel:"+args[0], "HyperSMS", smstext);
		System.out.println("MessageID: "+msg.getMessageId());
	}
}
