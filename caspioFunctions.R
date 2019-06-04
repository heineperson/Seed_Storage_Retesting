# Caspio Functions
caspio_get_all = function(TableName,login1){
  # This table reads the table defined by TableName into R
  # Query parameters specify the row limit (default = 100 & max = 1000)
  url1 = sprintf("https://c4axa460.caspio.com/rest/v1/tables/%s/rows/?q={%s:%s,limit:1000}",TableName,
                 shQuote("orderby"),shQuote("PK_ID"))
  # url1 = sprintf("https://c4axa460.caspio.com/rest/v1/tables/%s/rows/?q={limit:1000}",TableName)
  print(url1)
  table_get = GET(url = url1,
                  #url=paste("https://c4axa460.caspio.com/rest/v1/tables/",TableName,"/rows?q={limit:1000,'where':",query,"}",sep=""),
                  add_headers(Authorization=paste("Bearer", content(login1)$access_token),Accept="application/json"))
  #,query=list(Genus_ITIS="Abronia"))
  
  # This converts JSON Object to text
  table_json = content(table_get, as="text")
  # This converts JSON Object to dataframe
  table_dataframe = fromJSON(table_json,simplifyDataFrame=TRUE)
  #This converts dataframe to data.table
  table_DT = as.data.table(table_dataframe$Result)
  
  #while(dim(table_DT)[1]>=1000)
    return(table_DT)
  print(dim(table_DT))
  
}

