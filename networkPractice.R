require(chromote)

b <- ChromoteSession$new()

b$Debugger$enable()

b$Page$navigate("https://www.miamiherald.com")

b$Network$enable()

headers <- b$Network$requestWillBeSentExtraInfo()

b$Network$webSocketHandshakeResponseReceived()
c$continueInterceptedRequest(interceptionId)
