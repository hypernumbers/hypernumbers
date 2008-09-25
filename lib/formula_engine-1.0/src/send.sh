# Use as: sh send.sh +44xxxxxxxxxx "sms msg txt"

export CLASSPATH=.:Web21C-JavaSDK-5.2.1.jar:./lib/activation-1.1.jar:./lib/aspectjrt.jar:./lib/commons-codec-1.3.jar:./lib/commons-discovery-0.2.jar:./lib/commons-httpclient-3.0.1.jar:./lib/commons-lang-2.1.jar:./lib/commons-logging-1.0.4.jar:./lib/jaxb-api-2.0.jar:./lib/jaxb-impl-2.0.1.jar:./lib/jdom-1.0.jar:./lib/stax-api-1.0.1.jar:./lib/wsdl4j-1.5.2.jar:./lib/wss4j-1.5.0.jar:./lib/wstx-asl-2.9.3.jar:./lib/xalan-2.7.0.jar:./lib/xercesImpl.jar:./lib/xfire-all-1.2.3.jar:./lib/xfire-jsr181-api-1.0-M1.jar:./lib/xmlsec-1.3.0.jar
java ZhopaSms $1 $2
