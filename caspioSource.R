# Log in to Caspio Account
library(httr)

#EndPoint Token
end1 = "https://c4axa460.caspio.com/oauth/token"
ClientID = "12c7a92638664de53f445c344875b5a41961342259095fb71e"
ClientSecret = "f7ed5990169847d9b493a31bc2dbff68c464648439c9d33f53"

# Argument for POST function
Body = paste("grant_type=client_credentials&client_id=",ClientID,"&client_secret=",ClientSecret,sep="")

# Get Login Response
login1 = POST(url = end1, body=Body)