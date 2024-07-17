library(mongolite)
library(jsonlite)
library(rentrez)

pubmedDB = mongo(collection="articles", db="pubmed2")
print(pubmedDB$iterate()$one())

# list of pmids to retrieve
pmids <- c("24658839", "24658839", "24658839")
query <- sprintf('{"pmid": {"$in": %s}}', toJSON(pmids))
query <- gsub("\\\\", "", query)
print(query)
result <- pubmedDB$find('{\"pmid\":\"24658839\"}') #works
print(result$title)

# Create the list of pmids
#pmid_list <- list("36504274","36462524") #ids not found
pmid_list <- list("36504274", "24658839")#works

# Find the documents whose pmid field is in the list
allDocs <-data.frame(matrix(ncol = 4, nrow = 0))
columnNames <- c("pmid", "year", "title", "abstract")
colnames(allDocs) <- columnNames
for (id in pmid_list){

  print(id)
  #print(toString(id))
  query <- paste('{"pmid":"',toString(id),'"}',sep = "")
  #query <- gsub("/", "", query)
  print(query)
  article <- pubmedDB$find(query)
  #article <- pubmedDB$find('{"pmid":"24658839"}')
  print(article)
  allDocs[nrow(allDocs) + 1,] <- c(article$pmid,article$pubdate,article$title,article$abstract)
}
print(allDocs$pmid)

#Existing model
source('get_pubmed_data.R')
query<-"theodosiou t[AU]"
#query <- "drosophila"
#Fetch pubmed data (abstracts etc)
my_query <- get_pubmed_ids(query)
print(my_query$Count)
print(typeof(my_query$IdList))
print(my_query$IdList)
my_query_ids <- my_query$IdList[[1]]
my_query_ids <- unlist(my_query$IdList)
print(my_query_ids)
idList <- list()
for (i in 1:length(my_query$IdList)) {
  print(my_query$IdList[[i]])
  idList <- c(idList,my_query$IdList[[i]])
}
print(idList)

#Current function
pmids <- my_query
#print(length(pmids$IdList))
print(pmids$Count)
#The difference between pmids$Count and length(pmids$IdList) exists because pmids$IdList 
#is always a list of the 20 first ids (result of get_pubmed_ids)
#It is impossible to take all the pmids without the abstracts (that's what fetch_pubmed_data does)
#So local pubmed is useless

# Retrieve pubmed data in XML format given the PMIDs
pubmed_data_xml <- easyPubMed::fetch_pubmed_data(pmids)

# Store Pubmed Records as elements of a list
all_xml <- easyPubMed::articles_to_list(pubmed_data_xml)

# convert xml PubMed documents to data frame
df_of_articles <- do.call(rbind, lapply(all_xml, easyPubMed::article_to_df, max_chars = -1, getAuthors = FALSE))
print(df_of_articles$pmid)

#My model
allDocs <-data.frame(matrix(ncol = 4, nrow = 0))
columnNames <- c("pmid", "year", "title", "abstract")
colnames(allDocs) <- columnNames
#print(allDocs)
print(length(my_query$IdList))

#for (id in my_query$IdList){
onlineIds=0
localIds=0
for (id in df_of_articles$pmid){
  
  print(typeof(id))
  #print(toString(id))
  #query <- paste('{"pmid":"',toString(id),'"}',sep = "")
  query <- paste('{"pmid":"',id,'"}',sep = "")
  #query <- gsub("/", "", query)
  print(query)
  article <- pubmedDB$find(query)
  print(article)
  onlineIds=onlineIds+1
  if(nrow(article)>0) {
    localIds=localIds+1
    allDocs[nrow(allDocs) + 1,] <- c(article$pmid,article$pubdate,article$title,article$abstract)
  }

}
print(onlineIds)
print(localIds)
print(length(allDocs$pmid))

#Rentrez model
query<-"theodosiou t[AU]"
pmids<-entrez_search(db="pubmed", term = "drosophila", retmax=10000)$ids
pmidsCount<-entrez_search(db="pubmed", term = "drosophila")$count
print(pmidsCount)
print(length(pmids))

#Rentrez web history
pmidsWeb<-entrez_search(db="pubmed", term = "drosophila", use_history=TRUE)$web_history
print(pmidsWeb)
query_summ <- entrez_summary(db="pubmed", web_history=pmidsWeb)
print(query_summ)
#pmids <-extract_from_esummary(query_summ, "uid")

# Get the names of the esummary records
esummary_names <- names(query_summ)

# Extract the IDs from the esummary names
ids <- gsub("`", "", esummary_names)

# Print the list of IDs
print(ids)

# fetch data and save them in a data frame
df_of_articles <- get_pubmed_data(my_query)
print(colnames(df_of_articles))