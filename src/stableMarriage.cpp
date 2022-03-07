#include <iostream>
#include <unordered_map>
#include <vector>
#include <Rcpp.h>
using namespace Rcpp;

typedef std::unordered_map<std::string, std::vector<std::string> > prefTable;
typedef std::vector<std::vector<std::string> > matching;

prefTable listToPrefTable(List& pref)
{
  CharacterVector names = pref.names();
  prefTable out {};
  long int n {pref.size()};
  for(long int i = 0; i < n; i++)
  {
    out[as<std::string>(names[i])] = as<std::vector<std::string> >(pref[i]);
  }
  return out;
}

std::string createOmega(std::vector<std::string> strings)
{
    int n {1};
    for (std::string x : strings) if(n<x.length()) n = x.length();
    n++;
    std::string omega;
    for (int i = 1; i<=n; i++) omega += "a";
    return omega;
}

std::string findPartner(std::string name, matching match)
{
    int n = match[0].size();
    for (int i = 0; i < n; i++)
    {
        if(match[0][i] == name) return match[1][i];
        if(match[1][i] == name) return match[0][i];
    }
    throw std::invalid_argument("Name not found in matching");
}

std::vector<std::string> getPrefKeys(prefTable pref)
{
    std::vector<std::string> out{};
    for(auto &kv : pref) out.push_back(kv.first);
    return out;
}

matching getPrefValues(prefTable pref)
{
    matching out{};
    for(auto &kv : pref) out.push_back(kv.second);
    return out;
}

std::vector<std::string> removeFromList(std::vector<std::string> list, std::string item)
{
    std::vector<std::string> out {};
    for(std::string s : list) if(s!=item) out.push_back(s);
    return out;
}

matching stableMatchingC(prefTable pref1, prefTable pref2)
{
    int n = pref1.size();
    std::vector<std::string> men = getPrefKeys(pref1);
    std::vector<std::string> women = getPrefKeys(pref2);
    std::string omega {createOmega(men)};
    //initially match all women to omega
    matching match{{},{}};
    for(int i = 0; i < n; i++) match[0].push_back(omega);
    match[1] = getPrefKeys(pref2);
    
    //add omega to women's preferences
    for(auto& kv : pref2) kv.second.push_back(omega);
    
    int k = 0;
    std::string X;
    std::string x;
    std::vector<std::string> xList;
    std::string xPartner;
    
    while(k < n)
    {
        Rcpp::checkUserInterrupt();
        //X <- k+1th man
        X = men[k];
        
        while(X != omega)
        {
            //x <- X's best remaining preference
            x = pref1[X][0];
            
            //find if x prefers X to her current partner
            bool xPrefersX;
            xList = pref2[x];
            xPartner = findPartner(x, match);
            for(std::string man : xList)
            {
                if(man == xPartner)
                {
                    xPrefersX = false;
                    break;
                }
                else if(man == X)
                {
                    xPrefersX = true;
                    break;
                }
            }
            
            //if x prefers X, engage the two via the matching, and try to update the matching for x's previous partner
            if(xPrefersX)
            {
                for(int i = 0; i < n; i++)
                {
                    if (match[1][i] == x)
                    {
                        match[0][i] = X;
                        X = xPartner;
                        break;
                    }
                }
            }
            
            //if newly unpartnered men is not omega, remove his ex from his preference list, as she is incompatible
            if(X!=omega) pref1[X] = removeFromList(pref1[X],x);
        }
        k++;
    }
    return match;
}

std::string toString(std::vector<std::string> input)
{
    std::string out {"["};
    for(auto& str:input) out += str + ",";
    out.pop_back();
    return out + "]";
}

// [[Rcpp::export]]
std::vector<std::vector<std::string> > stableMatching(List& list1, List& list2)
{
  prefTable pref1{listToPrefTable(list1)};
  prefTable pref2{listToPrefTable(list2)};
  return stableMatchingC(pref1, pref2);
}
