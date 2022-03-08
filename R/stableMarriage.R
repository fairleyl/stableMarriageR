library("Rcpp")

#Preference tables will be represented as named lists of string vectors

isValidPrefTables = function(pref1, pref2)
{
  #check dimensions and duplicates and check all elements belong to opposite gender
  n = length(pref1)
  
  if(n!=length(pref2))
  {
    return(F)
  }
  
  names1 = names(pref1)
  names2 = names(pref2)
  
  for(l in pref1)
  {
    if(length(l)!=n || sum(duplicated(l)) > 0)
    {
      return(F)
    }
    
    for(name in l)
    {
      if(length(names2[names2 == name]) != 1)
      {
        return(F)
      }
    }
  }
  
  for(l in pref2)
  {
    if(length(l)!=n || sum(duplicated(l)) > 0)
    {
      return(F)
    }
    
    for(name in l)
    {
      if(length(names1[names1 == name]) != 1)
      {
        return(F)
      }
    }
  }
  
  T
}

#R wrapper function for main C++ algorithm
findStableMatching = function(pref1, pref2, checkTables = F)
{
  if(checkTables)
  {
    if(!isValidPrefTables(pref1, pref2))
    {
      stop("Invalid Preference Tables")
    }
  }
  stableMatching(pref1, pref2)
}
