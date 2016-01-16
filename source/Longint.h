#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <iterator>
#include <cstdio>
#include <cstdlib>
#include <cmath>
#include <cstring>
#include <ctime>
#include <string>
#include <algorithm>
#include <map>
#include <set>
#include <vector>
#include <queue>
#include <bitset>
#include <cctype>
#include <cassert>


using namespace std;
//==高精类加减乘除数组====================
int judge(char a[],int a1,char b[],int b1);
string div(string a,string b);
string add(string a, string b);
string mul(string a,string b);

string sub(string a, string b);

//==高精类============================
class Longint
{
private:
    static long long str2shortInt(string str, bool &flg)
    {
        char *tptr;
        int base = 10;
        const char *hptr=str.c_str();
        long long tmp = (long long)strtoll(hptr, &tptr, base);
        if (tptr == hptr || tptr != hptr + str.length())
        {
            flg = 0;
            return 0;
        }
        flg = true;
        return tmp;
    }
    static double str2double(string str, bool &flg)
    {
        char *tptr;
        const char *hptr=str.c_str();
        double tmp = strtod(hptr, &tptr);
        if (tptr == hptr || tptr != hptr + str.length())
        {
            flg = 0;
            return 0;
        }
        flg = true;
        return tmp;
    }
    static string lltostr(long long a)
    {
        stringstream strstr;
        strstr << a;
        string str(strstr.str());
        return str;
    }
public:
    string s;
    int len;
    bool sign;
    Longint(){s = "";len = 0;}
    Longint(const string &_s) {
        string tmp =_s;
        if (tmp.length()!= 1) {while (tmp[0] == '0'){tmp = tmp.substr(1,tmp.length()-1);}}
        if (tmp.length()>=2&&tmp[0] == '-')  {while (tmp[1] == '0'){tmp = tmp[0]+tmp.substr(2,tmp.length()-2);}}
        if (tmp == "-") {sign = 0; s = "0"; len = 1;}
        else
        if (tmp[0]=='-')
        {
            sign = 1;
            s = tmp.substr(1,tmp.length()-1);
            len  = s.length();
        }
        else
        {
            sign = 0;
            s = tmp;
            len = tmp.length();
        }
    }
    Longint(const long long &a)
    {
        string tmp =Longint::lltostr(a);
        if (tmp.length()!= 1) {while (tmp[0] == '0'){tmp = tmp.substr(1,tmp.length()-1);}}
        if (tmp == "-0") {sign = 0; s = "0"; len = 1;}
        else
        if (tmp[0]=='-')
        {
            sign = 1;
            s = tmp.substr(1,tmp.length()-1);
            len  = s.length();
        }
        else
        {
            sign = 0;
            s = tmp;
            len = tmp.length();
        }
    }
    Longint(const Longint &li) {s = li.s; len = li.len;sign = li.sign;}
    friend double Longint2double(const Longint &obj)
    {
        bool flg;
        if (obj.len <= 10)
        {
            if (obj.sign == 0) return (double) Longint::str2shortInt(obj.s,flg);
            else return (double) Longint::str2shortInt("-"+obj.s,flg);
        }
        else
        {
            string tmp = obj.s.substr(0,9);
            int a = Longint::str2shortInt(tmp,flg);
            if (obj.s[9] >= '5') a += 1;
            string a_str = Longint::lltostr(a);
            string res = a_str.substr(0,a_str.length()-8)+"."+a_str.substr(a_str.length()-8,a_str.length()-1)+"e"+Longint::lltostr(obj.s.length()-1);
            if (obj.sign == 1) return Longint::str2double("-"+res,flg);
            else return Longint::str2double(res,flg);
        }
    }

    Longint &operator = (const long long &a)
    {
        Longint tmp(a);
        s = tmp.s;
        len = tmp.len;
        sign =tmp.sign;
        return *this;
    }
    Longint operator - ()
    {
        return Longint(0) - *this;
    }
    friend istream& operator >> (istream &in, Longint& obj)
    {
        string tmp;
        in >> tmp;if (tmp.length()!= 1) {while (tmp[0] == '0'){tmp = tmp.substr(1,tmp.length()-1);}}
        obj.sign = 0;
        if (tmp[0] == '-') obj.s = tmp.substr(1,tmp.length()-1),obj.sign = 1;
        else  obj.s = tmp;
        obj.len = obj.s.length();
        return in;

    }
    friend ostream& operator << (ostream &os, const Longint &obj)
    {
        if (obj.sign == 1) os << "-";
        os << obj.s;
        return os;
    }
    friend bool operator == (const Longint &s1, const Longint &s2){return (s1.s == s2.s && s1.sign == s2.sign);}
    friend bool operator != (const Longint &s1, const Longint &s2){return !(s1 == s2);}

    friend bool operator < (const Longint &s1, const Longint &s2)
    {
        if (s1.sign != s2.sign )return s1.sign > s2.sign;
        else
        {
            bool f;
            if (s1.len != s2.len)
            {
                f = s1.len < s2.len;
                if (s1.sign == 1) f = 1 - f;
                return f;
            }
            else
            {
                f = s1.s < s2.s;
                if (s1.sign == 1) f = 1 - f;
                return f;
            }
        }
    }
    friend bool operator > (const Longint &s1, const Longint &s2){return s2 < s1;}
    friend Longint max(const Longint &s1,const Longint &s2) {return (s1 > s2) ? Longint(s1) : Longint(s2);}
    friend Longint min(const Longint &s1,const Longint &s2) {return (s1 < s2) ? Longint(s1) : Longint(s2);}
    friend Longint abs(const Longint &s1) {return Longint(s1.s);}
    friend Longint operator+ (const Longint &s1, const Longint &s2)
    {
        if (s1.sign == s2.sign)
        {
            if (s1.sign == 0)return Longint(add(s1.s,s2.s));
            else return Longint("-"+add(s1.s,s2.s));
        }
        else
        {
            Longint maxx = max(s1,s2), minx = min(s1,s2);
            if (maxx > abs(minx))
                return Longint(sub(maxx.s,minx.s));
            else return Longint("-"+sub(abs(minx).s,maxx.s));
        }
    }
    friend Longint operator- (const Longint &s1, const Longint &s2)
    {
        if (s1.sign != s2.sign)
        {
            if (s1.sign == 0)return Longint(add(s1.s,s2.s));
            else return Longint("-"+add(s1.s,s2.s));
        }
        else
        {
            if (s1.sign == 0)
            {
                return s1 > s2 ? Longint(sub(s1.s,s2.s)) : Longint("-"+sub(s2.s,s1.s));
            }
            else
            {
                return s1 > s2 ? Longint(sub(s2.s,s1.s)) : Longint("-"+sub(s1.s,s2.s));
            }
        }
    }
    friend Longint operator* (const Longint &s1, const Longint &s2)
    {
        if (s1.sign == s2.sign)return Longint(mul(s1.s,s2.s));
        else return Longint("-"+mul(s1.s,s2.s));
    }

    friend Longint operator/ (const Longint &s1, const Longint &s2)
    {
        if (s1.sign == s2.sign)return Longint(div(s1.s,s2.s));
        else return Longint("-"+div(s1.s,s2.s));
    }
    Longint &operator+= (const Longint &s1)
    {
        *this = *this + s1;
        return *this ;
    }
    Longint &operator-= (const Longint &s1)
    {
        *this = *this - s1;
        return *this ;
    }
    Longint &operator*= (const Longint &s1)
    {
        *this = *this * s1;
        return *this ;
    }
   Longint &operator/= (const Longint &s1)
    {
        *this = *this / s1;
        return *this ;
    }
    friend Longint operator% (const Longint &s1, const Longint &s2)
    {
        Longint _s1 (s1.s),_s2 (s2.s);
        Longint ans = _s1-_s1/_s2*_s2;


        if (s1.sign == 0)
        return ans;
        else return Longint(0)-ans;
    }

    friend Longint gcd (Longint m,Longint n)
    {
        if(m<n){Longint tmp=m;m=n,n=tmp;}
        if(n==Longint(0))return m;
        else return gcd(n,m%n);
    }

    friend Longint lcm (Longint m,Longint n)
    {
        if (m == 0 || n == 0) return 0;
        return (m*n)/gcd(m,n);
    }


    friend double operator+ (const Longint &s1, const double &s2)
    {
        return (double) Longint2double(s1) + s2;
    }
    friend double operator+ (const double &s1, const Longint &s2)
    {
        return (double) Longint2double(s2) + s1;
    }
    friend double operator- (const Longint &s1, const double &s2)
    {
        return (double) Longint2double(s1) - s2;
    }
    friend double operator- (const double &s1, const Longint &s2)
    {
        return (double) s1 - Longint2double(s2);
    }
    friend double operator* (const Longint &s1, const double &s2)
    {
        return (double) Longint2double(s1) * s2;
    }
    friend double operator* (const double &s1, const Longint &s2)
    {
        return (double) Longint2double(s2) * s1;
    }
    friend double operator/ (const Longint &s1, const double &s2)
    {
        return (double) Longint2double(s1) / s2;
    }
    friend double operator/ (const double &s1, const Longint &s2)
    {
        return (double) s1 / Longint2double(s2);
    }


};


