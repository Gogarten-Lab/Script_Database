#functions deriving a (potentially exact) Mantel-like test of whether values in a
#quantitative matrix differ between levels of a factor in a categorical matrix
#orders of rows and columns within and across the two matrices must be identical
#and the two matrices must be of the exact same size and both be quadratic
#all three functions are needed
#written by Roger Mundry
#version Apr. 04, 2013
#arguments of the function cat.mantel.test are:
#val.mat: numeric; the matrix with the quantitites
#cat.mat: numeric, character or factor; the matrix with the categorical variable
#part: character, the parts of the matrix to be used; can be one of "upper", "lower" or "all"
#with lower and upper refering to the lower left and upper right triangle, respectively
#exact.thresh: numeric; for samples larger than this value an approximate permutation test is applied
#(default is 9, and I recommend to stick with this)
#n.perm: number of permuations used in the approximate case
#(default is 10000, and I recommend to stick with this or enlarge it)
#val.mat, cat.mat: the two matrices to be correlated; are assumed to be square , with equal
#returns a list with three objects
#one is named 'result' and a data frame which should be self explaing
#the other functions are just needed by the main function, but the user doesn't need to worry about them

require(gtools)
cat.mantel.test<-function(val.mat, cat.mat, part, exact.thresh=9, n.perm=1000, response=c("quantitative", "categorical"), test.stat=mean){
  response=response[1]
  if(is.data.frame(cat.mat)){
    cat.mat=matrix(as.character(unlist(cat.mat)), ncol=ncol(cat.mat), byrow=F)
  }
  if(is.data.frame(val.mat)){
    val.mat=matrix(as.numeric(as.character(unlist(val.mat))), ncol=ncol(val.mat), byrow=F)
  }
  if(part=="upper" | part=="lower"){
    val.mat=convert.mat(val.mat, part)
    cat.mat=convert.mat(cat.mat, part)
  }
  if(response=="quantitative"){
    orig=measure.test.stat(v.mat=val.mat, c.mat=cat.mat, part=part, test.stat=test.stat)
  }else{
    orig=data.frame(value=0)
    orig$value=measure.test.stat.cat.resp(v.mat=val.mat, c.mat=cat.mat, part=part)
  }
  mat.size=ncol(cat.mat)
  if(mat.size>exact.thresh){
    appr=1
    all.res=orig$value
  }else{
    perms=permutations(mat.size,mat.size)
    n.perm=nrow(perms)
    all.res=c()
    appr=0
  }
  for (i in 1:(n.perm-appr)){
    if (appr==1){
      new.ind=sample(1:mat.size, mat.size, replace=F)
    }else{
      new.ind=perms[i,]
    }
    ran.mat=cat.mat[new.ind, new.ind]
    if(response=="quantitative"){
      all.res=rbind(all.res, measure.test.stat(v.mat=val.mat, c.mat=ran.mat, part=part, test.stat=test.stat)$value)
    }else{
      all.res=rbind(all.res, measure.test.stat.cat.resp(v.mat=val.mat, c.mat=ran.mat, part=part))
    }
  }
  orig=data.frame(orig, P=apply(all.res, 2, function(x){return(sum(x>=x[1]))})/n.perm)
  if(appr==1){method="approximate"}else{method="exact"}
  if(response=="quantitative"){
    return(list(result=orig, method=method, n.perm=n.perm, response=response))
  }else{
    list(result=orig, method=method, n.perm=n.perm, response=response)
  }
}

measure.test.stat<-function(v.mat, c.mat, part, test.stat){
  if(part=="all-diag"){
    v.mat=c(v.mat[lower.tri(v.mat)],v.mat[upper.tri(v.mat)])
    c.mat=c(c.mat[lower.tri(c.mat)],c.mat[upper.tri(c.mat)])
  }else if(part=="all+diag"){
    v.mat=unlist(c(v.mat))
    c.mat=unlist(c(c.mat))
  }else if(part=="lower"){
    v.mat=as.vector(v.mat[lower.tri(v.mat)])
    c.mat=as.vector(c.mat[lower.tri(c.mat)])
  }else if(part=="upper"){
    v.mat=as.vector(v.mat[upper.tri(v.mat)])
    c.mat=as.vector(c.mat[upper.tri(c.mat)])
  }
  xres=tapply(v.mat, c.mat, FUN=test.stat)
  total=sum((xres-mean(xres))^2)
  i.res=abs(outer(xres, xres, "-"))
  i.res.names=outer(names(xres), names(xres), function(x, y){paste(x, y, sep=" vs. ")})
  return(data.frame(comparison=c("all", i.res.names[lower.tri(i.res.names)]), value=c(total, i.res[lower.tri(i.res)])))
}

measure.test.stat.cat.resp<-function(v.mat, c.mat, part){
  if(part=="all-diag"){
    v.mat=c(v.mat[lower.tri(v.mat)],v.mat[upper.tri(v.mat)])
    c.mat=c(c.mat[lower.tri(c.mat)],c.mat[upper.tri(c.mat)])
  }else if(part=="all+diag"){
    v.mat=unlist(c(v.mat))
    c.mat=unlist(c(c.mat))
  }else if(part=="lower"){
    v.mat=as.vector(v.mat[lower.tri(v.mat)])
    c.mat=as.vector(c.mat[lower.tri(c.mat)])
  }else if(part=="upper"){
    v.mat=as.vector(v.mat[upper.tri(v.mat)])
    c.mat=as.vector(c.mat[upper.tri(c.mat)])
  }
  xx=table(v.mat, c.mat)
  E=outer(rowSums(xx), colSums(xx), "*")/sum(xx)
  return(sum(((xx-E)^2)/E))
}

convert.mat<-function(mat, part){
  if (part=="lower"){
    xxt=t(mat)
    xxt[lower.tri(xxt)==T]=mat[lower.tri(mat)==T]
    mat=xxt
  }else if(part=="upper"){
    xxt=t(mat)
    mat[lower.tri(mat)==T]=xxt[lower.tri(xxt)==T]
  }
  return(mat)
}

