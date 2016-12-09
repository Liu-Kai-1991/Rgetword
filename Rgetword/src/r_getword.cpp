#include<iostream>
#include<string>
#include<vector>
#include<set>
#include<algorithm> 
#include<Rcpp.h>
using namespace std;
using namespace Rcpp;

// Tkey is a class contains an id and two keys.
class Tkey
{
public:
    int id;
    int key[2];
    bool operator < (const Tkey &x) const
        {       
            if (key[0] < x.key[0]) return 1;
            if (key[0] > x.key[0]) return 0;
            if (key[1] < x.key[1]) return 1;
            else return 0;
        }
};

// Check whether a string is a substring of another string
bool instring(string &x, string &y)
{
    if (y.length()<x.length()) return 0;    
    for (int i = y.length() - x.length(); i>=0; i--)
    {
        bool flag = 1;
        for (int j = 0; j<x.length(); j++)
            if (x[j]!=y[i+j])
            {
                flag = 0;
                break;
            }
        if (flag) return 1;
    }
    return 0;
}

// Every Chinese character encoded in utf8 is group of 3 char types in C++
// This function convert a string into a list of strings. Each string in the list
// has a length of 3
vector<string> Chinese_String_Convert_utf8(string &str)
{
    vector<string> charlist;
    for (string::iterator i = str.begin(); i!= str.end(); i++)
    {
        string temp = "";       
        if (int(*i)<0) //If this char is encode as ASCII then do nothing
        {
            temp+=(*i);
            i++;
            temp+=(*i);
            i++;
            temp+=(*i);
        }
        else temp+=(*i);
        if (temp == " " || temp == "\t") continue;
        charlist.push_back(temp);
    }
    return charlist;
}

// Every Chinese character encoded in unicode is group of 2 char types in C++
// This function convert a string into a list of strings. Each string in the list
// has a length of 3
vector<string> Chinese_String_Convert_unicode(string &str)
{
    vector<string> charlist;
    for (string::iterator i = str.begin(); i!= str.end(); i++)
    {
        string temp = "";        
        if (int(*i)<0) //If this char is encode as ASCII then do nothing
        {
            temp+=(*i);
            i++;
            temp+=(*i);
        }
        else temp+=(*i);
        if (temp == " " || temp == "\t") continue;
        charlist.push_back(temp);
    }
    return charlist;
}

// Quick sort
void qsort(vector<string> &charlist, vector<int> &idlist, int l, int r)
{
    int i = l;
    int j = r;
    string x = charlist[idlist[(l+r)/2]];
    while (i<=j)
    {
        while (charlist[idlist[i]] < x) i++;
        while (charlist[idlist[j]] > x) j--;
        if (i<=j)
        {
            int temp = idlist[i];
            idlist[i] = idlist[j];
            idlist[j] = temp;
            i++;
            j--;
        }
    }
    if (i<r) qsort(charlist,idlist,i,r);
    if (j>l) qsort(charlist,idlist,l,j);    
}

// Sort the text according to the characters, and get the rank
vector<int> Get_Initial_Rank(vector<string> &charlist)
{
    vector<int> idlist;
    vector<int> rank;
    for (int i = 0; i < charlist.size(); i++)
        idlist.push_back(i);
    qsort(charlist,idlist,0,charlist.size()-1);
    rank.resize(charlist.size());
    rank[idlist[0]] = 0;
    for (int i = 1; i < idlist.size(); i++)
        if (charlist[idlist[i]] == charlist[idlist[i-1]])
            rank[idlist[i]] = rank[idlist[i-1]];
        else rank[idlist[i]] = rank[idlist[i-1]]+1;
    return rank;
}

void radixsort(vector<Tkey> &list, vector<Tkey> &order, int c)
{
    vector< vector<Tkey> > bucket;
    bucket.resize(list.size());
    int max = 0;
    for (int i = 0; i < list.size(); i++)
    {
        if (list[i].key[c]>max) max = list[i].key[c];
        bucket[list[i].key[c]].push_back(list[i]);
    }
    int p = 0;
    for (int i = 0; i <= max; i++)
        for (int j = 0; j < bucket[i].size(); j++)        
            order[p++] = bucket[i][j];
}

void rank_sort(int delta, vector<int> &rank)
{
    vector<Tkey> list,order;
    list.resize(rank.size());
    order.resize(rank.size());
    for (int i = 0; i < list.size(); i++)
    {
        list[i].key[0] = rank[i];
        if (i + delta < rank.size())
            list[i].key[1] = rank[i+delta];
        else list[i].key[1] = 0;
        list[i].id = i;
    }
    radixsort(list,order,1);
    radixsort(order,list,0);
    rank[list[0].id] = 0;
    for (int i = 1; i < list.size(); i++)
        if (list[i-1]<list[i]) 
            rank[list[i].id] = rank[list[i-1].id] + 1;
        else rank[list[i].id] = rank[list[i-1].id];
}

// Construct the suffix array
vector<int> doubling(vector<int> &rank)
{
    rank_sort(0,rank);
    int delta = 1;
    while (delta< rank.size())
    {
        rank_sort(delta,rank);
        delta*=2;
    }
    vector<int> sa;
    sa.resize(rank.size());
    for (int i = 0; i < rank.size(); i++) 
        sa[rank[i]] = i;
    return sa;
}

// Height is length of repeated patterns
vector<int> getheight(vector<int> &rank, vector<string> &charlist, vector<int> &sa)
{
    vector<int> h;
    vector<int> height;
    h.resize(rank.size());
    height.resize(rank.size());
    for (int i=0;i<rank.size();i++)
    {
        if (rank[i] == 0 || i == 0 || h[i-1]<=1)
            h[i] = 0;
        else h[i] = h[i-1]-1;
        while (rank[i]>0 && i+h[i]<charlist.size() && sa[rank[i]-1] + h[i] < charlist.size() && charlist[i+h[i]] == charlist[sa[rank[i]-1] + h[i]])
            h[i]++;        
        height[rank[i]] = h[i];
    }
    return height;
}


// Compare the length of two string
bool cmp_string_length(const string &x,const string &y) 
{
    return x.length() > y.length();
}


// This is the C++ implementation for word segmentation
//
//min_l: integer, minimum length of the word
//max_l: integer, maximum length of the word
//exclude_in: bool, whether exclude words as a substring of other words 
vector<string> c_getword(string str, int min_l, int max_l, bool exclude_in)
{
    vector<string> charlist = Chinese_String_Convert_utf8(str); 
  
    //Construct the suffix array
    vector<int> rank = Get_Initial_Rank(charlist);
    vector<int> sa = doubling(rank);
    vector<int> height = getheight(rank,charlist,sa);
    set<string> hash;
    
    //Find repeated patterns as words
    for (int i =1; i < sa.size(); i++)
    {           
        if (height[i]<min_l || height[i]>max_l) continue;
        string temp;
        for (int j = sa[i]; j<sa[i]+height[i];j++)
            temp+=charlist[j];
        hash.insert(temp);
    }    
    vector<string> result;
    for (set<string>::iterator i = hash.begin(); i!=hash.end(); i++)    
        result.push_back((*i));
    
    //Delelte the words that are substring of other words 
    if (exclude_in) 
    {
        sort(result.begin(), result.end(), cmp_string_length);
        vector<string> result_ex;
        for (vector<string>::iterator i = result.begin(); i!=result.end(); i++)
        {
            bool flag = 0;
            for (vector<string>::iterator j = result_ex.begin(); j!=result_ex.end();j++)            
                if (instring((*i),(*j)))
                {
                    flag = 1;
                    break;
                }
            if (!flag) result_ex.push_back((*i));
        }
        return result_ex;
    }
    return result;
}

// [[Rcpp::export]]
//This is the R interface for the C++ function
//
//text: The input text
//min_l: integer, minimum length of the word
//max_l: integer, maximum length of the word
//para: bool, whether exclude words as a substring of other words 
CharacterVector r_getword(SEXP text, SEXP min_l, SEXP max_l, SEXP para) 
{
    bool exclude_in = Rcpp::as<bool>(para);
    int min_length = Rcpp::as<int>(min_l);
    int max_length = Rcpp::as<int>(max_l);
    std::string str = Rcpp::as<std::string>(text); 
    vector<string> result = c_getword(str,min_length,max_length,exclude_in);    
    CharacterVector out(result.size());
    for (int i =0;i<result.size();i++)
    {
      out[i] = result[i];
    }
    return out;
}
