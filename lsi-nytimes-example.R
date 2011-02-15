library('Matrix')
#text clustering using LSI
# LSI finds groups of co-occurring text using singular value decomposition
# can be used for clustering the co-occurring text as well as dimensional reduction of the documents
# tommy chheng
#
#docid.wordid.count.filename is a tsv file with doc.id	word.id	word.count
#vocab.filename is the vocab with each line number as the word Id
#k is the # of clusters to find
lsi = function(docid.wordid.count.filename, vocab.filename, k) {
	#columns in file are : doc.id	word.id	word.count
	doc.term.input <- as.matrix(read.table(docid.wordid.count.filename))
	
	#TODO: an improvement is to compute tfidf rather than just use tf
	doc.term.matrix <- sparseMatrix(doc.term.input[,1],doc.term.input[,2],x=doc.term.input[,3])
	
	# n = # of documents, m = # of terms, r = # of singular values
	#svdOutput$u is the decomposed document matrix: size n x r
	#diag(svdOutput$d) is the diagonal singular values in sorted order: size r x r
	#svdOutput$v is the decomposed term matrix: size r x m
	svd.output <- svd(doc.term.matrix)
	
	#read word mapping file
	term.map <- as.matrix(read.table(vocab.filename))
	
	for(i in 1:k){
		terms.in.cluster <- sort(t(svd.output$v[,i]), index.return=TRUE)
		
		#take the top 10 words from cluster
		top.terms <- terms.in.cluster$ix[1:10]
		print(term.map[top.terms])
	}
	
	print("----")
	#TODO: the u matrix should cluster docs
	for(i in 1:k){
  	docs.in.cluster <- sort(t(svd.output$u[i, ]), index.return=TRUE)
	}
}

lsi('nytimes.docIDwordIDcount.txt','nytimes.vocab.txt', 10)
